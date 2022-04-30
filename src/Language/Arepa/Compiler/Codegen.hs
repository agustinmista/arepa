module Language.Arepa.Compiler.Codegen
  ( LLVMModule
  , emitLLVM
  , renderLLVM
  ) where

import GHC.Exts
import Control.Monad.State

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Data.Map (Map)
import Data.Map qualified as Map

import Prettyprinter


import LLVM.AST          qualified as LLVM
import LLVM.AST.Type     qualified as LLVM
import LLVM.AST.Constant qualified as Constant

import LLVM.IRBuilder(MonadModuleBuilder, MonadIRBuilder, ModuleBuilderT, buildModuleT)
import LLVM.IRBuilder qualified as IR
import LLVM.Pretty

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad
import Language.TIM


----------------------------------------
-- Code generation
----------------------------------------

type LLVMModule = LLVM.Module

emitLLVM :: MonadArepa m => CodeStore -> m LLVMModule
emitLLVM store = do
  let name = fromName (store_name store)
  runLLVM name $ do
    registerGlobals store
    emitRTS
    emitPrimitives
    emitCodeStore store

renderLLVM :: MonadArepa m => LLVMModule -> m Text
renderLLVM m = return (ppllvm m)

----------------------------------------
-- Code generation internal state
----------------------------------------

data CodegenState = CodegenState {
  cg_globals :: Map Name LLVM.Operand,   -- Global operands
  cg_context :: [Map Name LLVM.Operand], -- Local operands
  cg_strings :: Map Text LLVM.Operand   -- Global strings
}

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState {
  cg_globals = mempty,
  cg_context = mempty,
  cg_strings = mempty
}

----------------------------------------
-- Code generation monads
----------------------------------------

-- The LLVM module builder monad
type LLVM m a = ModuleBuilderT (StateT CodegenState m) a

-- The constraints needed to write functions
type MonadLLVM m = (
    MonadArepa m,
    MonadState CodegenState m,
    MonadModuleBuilder m
  )

runLLVM :: MonadArepa m => String -> LLVM m a -> m LLVMModule
runLLVM name mb = evalStateT (buildModuleT (fromString name) mb) emptyCodegenState

----------------------------------------
-- Global operands

-- Register the operand associated to a global identifier
-- registerGlobalOperand :: MonadLLVM m => Ident -> LLVM.Operand -> m ()
registerGlobalOperand :: MonadLLVM m => Name -> LLVM.Operand -> m ()
registerGlobalOperand name op = do
  modify' $ \st -> st { cg_globals = Map.insert name op (cg_globals st) }

-- Get the operand associated with an identifier
-- lookupGlobalOperand :: MonadLLVM m => Ident -> m LLVM.Operand
lookupGlobalOperand :: MonadLLVM m => Name -> m LLVM.Operand
lookupGlobalOperand name = do
  globals <- gets cg_globals
  case Map.lookup name globals of
    Nothing -> throwInternalError ("lookupGlobalOperand: operand for global" <+> pretty name <+> "does not exist")
    Just op -> return op

----------------------------------------
-- Local operands

-- -- Register the operand associated to a variable identifier in the closest context
-- registerVarOperand :: MonadLLVM m => Name -> LLVM.Operand -> m ()
-- registerVarOperand name op = do
--   ctx <- gets cg_context
--   case ctx of
--     [] -> throwInternalError "registerVarOperand: null context"
--     c:cs -> modify' $ \st -> st { cg_context = Map.insert name op c : cs }

-- -- Get the operand associated with an identifier
-- lookupVarOperand :: MonadLLVM m => Name -> m LLVM.Operand
-- lookupVarOperand name = do
--   gets cg_context >>= search
--   where
--     search []     = throwInternalError ("lookupVarOperand: operand for variable" <+> pretty name <+> "does not exist")
--     search (c:cs) = maybe (search cs) return (Map.lookup name c)

-- -- Run the code inside of a new local context
-- insideLocalContext :: MonadLLVM m => m a -> m a
-- insideLocalContext ma = do
--   ctx <- gets cg_context
--   modify' $ \st -> st { cg_context = Map.empty : ctx }
--   a <- ma
--   modify' $ \st -> st { cg_context = ctx }
--   return a

----------------------------------------
-- Strings literals

-- Register a global string literal
-- If it is already registered, then return the corresponding operand
registerString :: MonadLLVM m => Text -> m LLVM.Operand
registerString str = do
  strings <- gets cg_strings
  case Map.lookup str strings of
    Just op -> return op
    Nothing -> do
      let name = mkGlobalStringName (Map.size strings)
      con <- IR.globalStringPtr (Text.unpack str) name
      let op = LLVM.ConstantOperand con
      modify' $ \st -> st { cg_strings = Map.insert str op strings }
      return op

----------------------------------------
-- Primitives

lookupPrim :: MonadLLVM m => Name -> m Prim
lookupPrim name = do
  case Map.lookup name primitives of
    Nothing ->
      throwInternalError ("lookupPrim: cannot find primitive operation " <> fromName name)
    Just prim ->
      return prim


----------------------------------------
-- Emitting RTS code
----------------------------------------

emitRTS :: MonadLLVM m => m ()
emitRTS = do
  debug "Emitting RTS externs"

  -- RTS evaluation operations
  forM_ rtsFunctions $ \(name, argTypes, retType) -> do
    op <- IR.extern (fromName name) argTypes retType
    registerGlobalOperand name op

  -- RTS main wrapper
  void $ IR.function "main" [] intType $ \[] -> do
    callVoidRTS "tim_start" []
    name <- lookupCompilerOption optEntryPoint
    entry <- lookupGlobalOperand name
    IR.call entry []
    IR.ret (IR.int32 0)


rtsFunctions :: [(Name, [LLVM.Type], LLVM.Type)]
rtsFunctions = [
    ("tim_start",                  [],                  voidType),
    ("tim_take",                   [longType],          voidType),
    ("tim_push_argument_argument", [longType],          voidType),
    ("tim_push_argument_int",      [valueType IntT],    voidType),
    ("tim_push_argument_double",   [valueType DoubleT], voidType),
    ("tim_push_argument_string",   [valueType StringT], voidType),
    ("tim_push_argument_label",    [funPtrType],        voidType),
    ("tim_push_value_int",         [valueType IntT],    voidType),
    ("tim_push_value_double",      [valueType DoubleT], voidType),
    ("tim_push_value_string",      [valueType StringT], voidType),
    ("tim_pop_value_int",          [],                  LLVM.ptr (valueType IntT)),
    ("tim_pop_value_double",       [],                  LLVM.ptr (valueType DoubleT)),
    ("tim_pop_value_string",       [],                  LLVM.ptr (valueType StringT)),
    ("tim_enter_argument",         [longType],          voidType),
    ("tim_enter_int",              [valueType IntT],    voidType),
    ("tim_enter_double",           [valueType DoubleT], voidType),
    ("tim_enter_string",           [valueType StringT], voidType),
    ("tim_enter_label",            [funPtrType],        voidType),
    ("tim_return",                 [],                  voidType)
  ]


callRTS :: (MonadLLVM m, MonadIRBuilder m) => Name -> [LLVM.Operand] -> m LLVM.Operand
callRTS name args = do
  fun <- lookupGlobalOperand name
  IR.call fun [ (arg, []) | arg <- args ]

callVoidRTS :: (MonadLLVM m, MonadIRBuilder m) => Name -> [LLVM.Operand] -> m ()
callVoidRTS name args = void (callRTS name args)

----------------------------------------
-- Emitting primitive operations
----------------------------------------

emitPrimitives :: MonadLLVM m => m ()
emitPrimitives = do
  forM_ (Map.toList primitives) $ \(name, prim) -> do
    let argTypes = valueType <$> fst (prim_type prim)
    let retType  = valueType (snd (prim_type prim))
    op <- IR.extern (fromName name) argTypes retType
    registerGlobalOperand name op

----------------------------------------
-- Emitting user code
----------------------------------------

-- Register global declarations before we start emitting code so out-of-order
-- and mutually-recursive functions can be generated using `IR.function`.
--
-- VERY IMPORTANT: this only works because the operand name used by `extern` and
-- `function` is the same as the name of the global, no fresh name is generated.
registerGlobals :: MonadLLVM m => CodeStore -> m ()
registerGlobals store = do
  forM_ (Map.keys (store_blocks store)) $ \name -> do
    let ty = LLVM.ptr voidFunType
    let op = mkGlobalOperand (mkMangledFunctionName name) ty
    registerGlobalOperand name op


-- Code stores
emitCodeStore :: MonadLLVM m => CodeStore -> m ()
emitCodeStore store = do
  forM_ (Map.toList (store_blocks store)) $ \(name, code) -> do
    emitCodeBlock name code


-- Code blocks
emitCodeBlock :: MonadLLVM m => Name -> CodeBlock -> m ()
emitCodeBlock name code = void $ do
  let funName = mkMangledFunctionName name
  IR.function funName [] voidType $ \[] -> do
    mapM_ emitInstr (toList code)


-- Instructions
emitInstr :: (MonadIRBuilder m, MonadLLVM m) => Instr -> m ()
emitInstr instr = do
  case instr of
    -- Take
    TakeArgI n -> do
      callVoidRTS "tim_take" [mkLong n]
    -- Enter
    EnterI (ArgM n) -> do
      callVoidRTS "tim_enter_argument" [mkLong n]
    EnterI (LabelM name) -> do
      fun <- lookupGlobalOperand name
      callVoidRTS "tim_enter_label" [fun]
    EnterI (ValueM (IntV n)) -> do
      int <- IR.bitcast (mkIntV n) (valueType IntT)
      callVoidRTS "tim_enter_value_int" [int]
    EnterI (ValueM (DoubleV n)) -> do
      double <- IR.bitcast (mkDoubleV n) (valueType DoubleT)
      callVoidRTS "tim_enter_value_double" [double]
    EnterI (ValueM (StringV s)) -> do
      op <- registerString s
      string <- IR.bitcast op (valueType StringT)
      callVoidRTS "tim_enter_value_string" [string]
    EnterI (ValueM (VoidV _)) -> do
      throwInternalError "emitInstr: impossible! cannot enter a void argument"
    -- Push arguments
    PushArgI (ArgM n) -> do
      callVoidRTS "tim_push_argument_argument" [mkLong n]
    PushArgI (LabelM name) -> do
      fun <- lookupGlobalOperand name
      callVoidRTS "tim_push_argument_label" [fun]
    PushArgI (ValueM (IntV n)) -> do
      int <- IR.bitcast (mkIntV n) (valueType IntT)
      callVoidRTS "tim_push_argument_int" [int]
    PushArgI (ValueM (DoubleV n)) -> do
      double <- IR.bitcast (mkDoubleV n) (valueType DoubleT)
      callVoidRTS "tim_push_argument_double" [double]
    PushArgI (ValueM (StringV s)) -> do
      op <- registerString s
      string <- IR.bitcast op (valueType StringT)
      callVoidRTS "tim_push_argument_string" [string]
    PushArgI (ValueM (VoidV _)) -> do
      throwInternalError "emitInstr: impossible! cannot push a void argument"
    -- Push values
    PushValueI FramePtrM -> do
      throwInternalError "emitInstr: impossible! we never generate instructions masking the frame pointer"
    PushValueI (InlineM (IntV n)) -> do
      int <- IR.bitcast (mkIntV n) (valueType IntT)
      callVoidRTS "tim_push_value_int" [int]
    PushValueI (InlineM (DoubleV n)) -> do
      double <- IR.bitcast (mkDoubleV n) (valueType DoubleT)
      callVoidRTS "tim_push_value_double" [double]
    PushValueI (InlineM (StringV s)) -> do
      op <- registerString s
      string <- IR.bitcast op (valueType StringT)
      callVoidRTS "tim_push_value_string" [string]
    PushValueI (InlineM (VoidV _)) -> do
      throwInternalError "emitInstr: impossible! cannot push a void value"
    -- Call
    CallI name -> do
      -- Find the primitive function operand
      fun <- lookupGlobalOperand name
      -- Pop the arguments
      prim <- lookupPrim name
      let (argTypes, retType) = prim_type prim
      args <- forM argTypes $ \case
        IntT -> do
          ptr <- callRTS "tim_pop_value_int" []
          IR.load ptr 0
        DoubleT -> do
          ptr <- callRTS "tim_pop_value_double" []
          IR.load ptr 0
        StringT -> do
          ptr <- callRTS "tim_pop_value_string" []
          IR.load ptr 0
        VoidT -> do
          throwInternalError "emitInstr: impossible! cannot pop a void value"
      -- Call the function
      res <- IR.call fun [ (arg, []) | arg <- args ]
      -- Push the return type
      case retType of
        IntT -> do
          callVoidRTS "tim_push_value_int" [res]
        DoubleT -> do
          callVoidRTS "tim_push_value_double" [res]
        StringT -> do
          callVoidRTS "tim_push_value_string" [res]
        VoidT -> do
          return ()
    -- Return
    ReturnI -> do
      callVoidRTS "tim_return" []


----------------------------------------
-- Low-level utilities
----------------------------------------

mkGlobalStringName :: Int -> LLVM.Name
mkGlobalStringName n = LLVM.mkName ("__string__." <> show n)

mkGlobalOperand :: LLVM.Name -> LLVM.Type -> LLVM.Operand
mkGlobalOperand name ty = LLVM.ConstantOperand (Constant.GlobalReference ty name)

mkMangledFunctionName :: Name -> LLVM.Name
mkMangledFunctionName name = LLVM.mkName ("__sc_" <> fromName (zEncode name) <> "__")

mkLong :: Int -> LLVM.Operand
mkLong n = IR.int64 (fromIntegral n)

mkIntV :: Int -> LLVM.Operand
mkIntV n = IR.int64 (fromIntegral n)

mkDoubleV :: Double -> LLVM.Operand
mkDoubleV = IR.double

----------------------------------------
-- LLVM types
----------------------------------------

voidType :: LLVM.Type
voidType = LLVM.VoidType

voidFunType :: LLVM.Type
voidFunType = LLVM.FunctionType voidType [] False

funPtrType :: LLVM.Type
funPtrType = LLVM.ptr voidFunType

valueType :: Type -> LLVM.Type
valueType IntT    = LLVM.i64
valueType DoubleT = LLVM.double
valueType StringT = LLVM.ptr LLVM.i8
valueType VoidT   = LLVM.VoidType

-- NOTE: the ones below are platform dependent!

intType :: LLVM.Type
intType = LLVM.IntegerType 32

longType :: LLVM.Type
longType = LLVM.IntegerType 64
