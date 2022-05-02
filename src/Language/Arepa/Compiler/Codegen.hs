module Language.Arepa.Compiler.Codegen
  ( LLVMModule
  , emitLLVM
  , renderLLVM
  ) where

import GHC.Exts
import Control.Monad.State

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Data.Maybe

import Data.Map (Map)
import Data.Map qualified as Map

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
  -- cg_context :: [Map Name LLVM.Operand], -- Local operands
  cg_strings :: Map Text LLVM.Operand   -- Global strings
}

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState {
  cg_globals = mempty,
  -- cg_context = mempty,
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
registerGlobalOperand :: MonadLLVM m => Name -> LLVM.Operand -> m ()
registerGlobalOperand name op = do
  whenVerbose $ debug ("Registering global operand " <> prettyPrint op <> " as " <> prettyPrint name)
  modify' $ \st -> st { cg_globals = Map.insert name op (cg_globals st) }

-- Get the operand associated with an identifier
-- NOTE: this emits an extern if the identifier is not a global defined here
lookupGlobalOperand :: MonadLLVM m => Name -> m LLVM.Operand
lookupGlobalOperand name = do
  whenVerbose $ debug ("Looking up global operand of " <> prettyPrint name)
  globals <- gets cg_globals
  case Map.lookup name globals of
    Nothing -> do
      warning $ "Emitting implicit extern for " <> prettyPrint name
      IR.extern (mkMangledFunctionName name) [] LLVM.void
    Just op -> do
      whenVerbose $ debug ("Found operand for " <> prettyPrint name <> ": " <> prettyPrint op)
      return op

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
  whenVerbose $ dump "Registering string" (prettyPrint str)
  strings <- gets cg_strings
  case Map.lookup str strings of
    Just op -> do
      whenVerbose $ debug ("The string already had a global operand: " <> prettyPrint op)
      return op
    Nothing -> do
      let name = mkGlobalStringName (Map.size strings)
      con <- IR.globalStringPtr (Text.unpack str) name
      let op = LLVM.ConstantOperand con
      modify' $ \st -> st { cg_strings = Map.insert str op strings }
      whenVerbose $ debug ("Created a new global operand: " <> prettyPrint op)
      return op

----------------------------------------
-- Primitives

lookupPrim :: MonadLLVM m => Name -> m Prim
lookupPrim name = do
  whenVerbose $ debug ("Looking up primitive operation " <> prettyPrint name)
  case Map.lookup name primitives of
    Nothing -> do
      throwInternalError ("lookupPrim: cannot find primitive operation " <> prettyPrint name)
    Just prim -> do
      whenVerbose $ dump ("Found primitive operation " <> prettyPrint name) (prettyShow (prim_arity prim, prim_type prim))
      return prim


----------------------------------------
-- Emitting RTS code
----------------------------------------

emitRTS :: MonadLLVM m => m ()
emitRTS = do

  -- RTS evaluation operations
  whenVerbose $ debug "Emitting RTS externs"
  forM_ rtsFunctions $ \(name, argTypes, retType) -> do
    op <- IR.extern (fromName name) argTypes retType
    registerGlobalOperand name op

  -- RTS main wrapper (only when necessary)
  whenVerbose $ debug "Emitting RTS main()"
  output <- lookupCompilerOption optOutput
  when (isJust output) $ do
    entry <- lookupCompilerOption optEntryPoint
    emitMain (mkName (fromMaybe "main" entry))


emitMain :: MonadLLVM m => Name -> m ()
emitMain name = do
  void $ IR.function "main" [] LLVM.i32 $ \[] -> do
    callVoidRTS "tim_start" []
    fun <- lookupGlobalOperand name
    IR.call fun []
    IR.ret (IR.int32 0)


rtsFunctions :: [(Name, [LLVM.Type], LLVM.Type)]
rtsFunctions = [
    ("tim_start",                  [],            LLVM.void),
    ("tim_take",                   [LLVM.i64],    LLVM.void),
    ("tim_push_argument_argument", [LLVM.i64],    LLVM.void),
    ("tim_push_argument_int",      [intVType],    LLVM.void),
    ("tim_push_argument_double",   [doubleVType], LLVM.void),
    ("tim_push_argument_string",   [stringVType], LLVM.void),
    ("tim_push_argument_label",    [funPtrType],  LLVM.void),
    ("tim_push_value_int",         [intVType],    LLVM.void),
    ("tim_push_value_double",      [doubleVType], LLVM.void),
    ("tim_push_value_string",      [stringVType], LLVM.void),
    ("tim_pop_value_int",          [],            LLVM.ptr intVType),
    ("tim_pop_value_double",       [],            LLVM.ptr doubleVType),
    ("tim_pop_value_string",       [],            LLVM.ptr stringVType),
    ("tim_enter_argument",         [LLVM.i64],    LLVM.void),
    ("tim_enter_int",              [intVType],    LLVM.void),
    ("tim_enter_double",           [doubleVType], LLVM.void),
    ("tim_enter_string",           [stringVType], LLVM.void),
    ("tim_enter_label",            [funPtrType],  LLVM.void),
    ("tim_return",                 [],            LLVM.void)
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
  whenVerbose $ debug "Emitting primitive operations externs"
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
  whenVerbose $ debug "Registering global definitions"
  forM_ (Map.keys (store_blocks store)) $ \name -> do
    let op = mkGlobalOperand (mkMangledFunctionName name) funPtrType
    registerGlobalOperand name op


-- Code stores
emitCodeStore :: MonadLLVM m => CodeStore -> m ()
emitCodeStore store = do
  whenVerbose $ debug ("Emitting code store " <> prettyPrint (store_name store))
  forM_ (Map.toList (store_blocks store)) $ \(name, code) -> do
    emitCodeBlock name code


-- Code blocks
emitCodeBlock :: MonadLLVM m => Name -> CodeBlock -> m ()
emitCodeBlock name code = void $ do
  whenVerbose $ debug ("Emitting code block " <> prettyPrint name)
  let funName = mkMangledFunctionName name
  IR.function funName [] LLVM.void $ \[] -> do
    mapM_ emitInstr (toList code)


-- Instructions
emitInstr :: (MonadIRBuilder m, MonadLLVM m) => Instr -> m ()
emitInstr instr = do
  whenVerbose $ dump "Emitting instruction" (prettyPrint instr)
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
      callVoidRTS "tim_enter_value_int" [mkIntV n]
    EnterI (ValueM (DoubleV n)) -> do
      callVoidRTS "tim_enter_value_double" [mkDoubleV n]
    EnterI (ValueM (StringV s)) -> do
      string <- registerString s
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
      callVoidRTS "tim_push_argument_int" [mkIntV n]
    PushArgI (ValueM (DoubleV n)) -> do
      callVoidRTS "tim_push_argument_double" [mkDoubleV n]
    PushArgI (ValueM (StringV s)) -> do
      string <- registerString s
      callVoidRTS "tim_push_argument_string" [string]
    PushArgI (ValueM (VoidV _)) -> do
      throwInternalError "emitInstr: impossible! cannot push a void argument"
    -- Push values
    PushValueI FramePtrM -> do
      throwInternalError "emitInstr: impossible! we never generate instructions masking the frame pointer"
    PushValueI (InlineM (IntV n)) -> do
      callVoidRTS "tim_push_value_int" [mkIntV n]
    PushValueI (InlineM (DoubleV n)) -> do
      callVoidRTS "tim_push_value_double" [mkDoubleV n]
    PushValueI (InlineM (StringV s)) -> do
      string <- registerString s
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

-- Name manipulation

mkGlobalOperand :: LLVM.Name -> LLVM.Type -> LLVM.Operand
mkGlobalOperand name ty = LLVM.ConstantOperand (Constant.GlobalReference ty name)

mkGlobalStringName :: Int -> LLVM.Name
mkGlobalStringName n = LLVM.mkName ("__string__." <> show n)

mkMangledFunctionName :: Name -> LLVM.Name
mkMangledFunctionName name = LLVM.mkName ("__sc_" <> fromName (zEncode name) <> "__")

-- Creating literal values

-- C long
mkLong :: Int -> LLVM.Operand
mkLong n = IR.int64 (fromIntegral n)

-- Arepa values
-- NOTE: make sure that these functions match the types defined below!

mkIntV :: Int -> LLVM.Operand
mkIntV n = IR.int64 (fromIntegral n)

mkDoubleV :: Double -> LLVM.Operand
mkDoubleV = IR.double

----------------------------------------
-- LLVM types
----------------------------------------

-- The type of a compiled supercombinator
-- In C: void (*f())
funPtrType :: LLVM.Type
funPtrType = LLVM.ptr (LLVM.FunctionType LLVM.void [] False)

----------------------------------------
-- Value types

valueType :: Type -> LLVM.Type
valueType IntT    = intVType
valueType DoubleT = doubleVType
valueType StringT = stringVType
valueType VoidT   = voidVType

-- NOTE: the ones below are platform dependent!

intVType :: LLVM.Type
intVType = LLVM.i64

doubleVType :: LLVM.Type
doubleVType = LLVM.double

stringVType :: LLVM.Type
stringVType = LLVM.ptr LLVM.i8

voidVType :: LLVM.Type
voidVType = LLVM.void


