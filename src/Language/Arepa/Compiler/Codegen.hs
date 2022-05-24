module Language.Arepa.Compiler.Codegen
  ( LLVMModule
  , emitLLVM
  , renderLLVM
  ) where

import GHC.Exts
import Control.Monad.Extra
import Control.Monad.State

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Data.Maybe

import Data.Map (Map)
import Data.Map qualified as Map

import LLVM.AST          qualified as LLVM
import LLVM.AST.Type     qualified as LLVM
import LLVM.AST.Constant qualified as Constant

import LLVM.IRBuilder(MonadModuleBuilder, MonadIRBuilder, ModuleBuilderT, buildModuleT, named)
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
renderLLVM m = do
  return (ppllvm m)

----------------------------------------
-- Code generation internal state
----------------------------------------

data CodegenState = CodegenState {
  cg_globals :: Map Name LLVM.Operand,  -- Global operands
  cg_strings :: Map Text LLVM.Operand,  -- Global strings
  cg_externs :: Map Name LLVM.Operand,
  cg_constrs :: Map Tag  LLVM.Operand
}

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState {
  cg_globals = mempty,
  cg_strings = mempty,
  cg_externs = mempty,
  cg_constrs = mempty
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
    MonadModuleBuilder m,
    MonadFix m
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
-- NOTE: this emits an extern if the identifier is not a global defined here (and if not --strict)
lookupGlobalOperand :: MonadLLVM m => Name -> m LLVM.Operand
lookupGlobalOperand name = do
  whenVerbose $ debug ("Looking up global operand of " <> prettyPrint name)
  globals <- gets cg_globals
  case Map.lookup name globals of
    Nothing -> do
      whenM hasStrictEnabled $ do
        throwInternalError ("lookupGlobalOperand: missing operand for " <> prettyPrint name <> " (strict mode is enabled)")
      registerExtern name
    Just op -> do
      whenVerbose $ debug ("Found operand for " <> prettyPrint name <> ": " <> prettyPrint op)
      return op

----------------------------------------
-- Strings literals

-- Register a global string literal
-- If it is already registered, then return the corresponding operand
registerString :: MonadLLVM m => Text -> m LLVM.Operand
registerString str = do
  whenVerbose $ dump "Registering string" str
  strings <- gets cg_strings
  case Map.lookup str strings of
    Just op -> do
      whenVerbose $ debug ("The string already had a global operand: " <> prettyPrint op)
      return op
    Nothing -> do
      let name = mkGlobalStringName (Map.size strings)
      op <- LLVM.ConstantOperand <$> IR.globalStringPtr (Text.unpack str) name
      modify' $ \st -> st { cg_strings = Map.insert str op strings }
      whenVerbose $ debug ("Created a new global string operand: " <> prettyPrint op)
      return op

----------------------------------------
-- External code

-- Register an extern for an out-of-scope variable
-- If it is already registered, then return the corresponding operand
registerExtern :: MonadLLVM m => Name -> m LLVM.Operand
registerExtern name = do
  whenVerbose $ dump "Registering extern" name
  externs <- gets cg_externs
  case Map.lookup name externs of
    Just op -> do
      whenVerbose $ debug ("The extern already had a global operand: " <> prettyPrint op)
      return op
    Nothing -> do
      warning $ "Emitting implicit extern for " <> prettyPrint name
      op <- IR.extern (mkBlockName name) [] voidType
      modify' $ \st -> st { cg_externs = Map.insert name op externs }
      whenVerbose $ debug ("Created a new global extern operand: " <> prettyPrint op)
      return op

----------------------------------------
-- Data constructors code

-- Register a data constructor code
-- If it is already registered, then return the corresponding operand
registerConstructorCode :: MonadLLVM m => Tag -> m LLVM.Operand
registerConstructorCode tag = do
  whenVerbose $ debug ("Registering data constructor " <> prettyPrint tag)
  constrs <- gets cg_constrs
  case Map.lookup tag constrs of
    Just op -> do
      whenVerbose $ debug ("The data constructor already had a global operand: " <> prettyPrint op)
      return op
    Nothing -> do
      let funName = mkConCodeName tag
      let op = mkGlobalOperand funPtrType funName
      IR.function funName [] voidType $ \[] -> do
        callVoidRTS "tim_data" [mkTag tag, op]
      modify' $ \st -> st { cg_constrs = Map.insert tag op constrs }
      whenVerbose $ debug ("Created a new global data constructor operand: " <> prettyPrint op)
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
      whenVerbose $ dump ("Found primitive operation " <> prettyPrint name) (prim_arity prim, prim_type prim)
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
  -- RTS main wrapper (only when necessary or forced)
  whenVerbose $ debug "Emitting RTS main()"
  output <- lookupCompilerOption optOutput
  forced <- hasEmitMainEnabled
  linkingDisabled <- hasLinkingDisabled
  when (isJust output || forced || not linkingDisabled) $ do
    entry <- lookupCompilerOption optEntryPoint
    emitMain (mkName entry)

emitMain :: MonadLLVM m => Name -> m ()
emitMain name = void $ do
  IR.function "main" [] LLVM.i32 $ \[] -> do
    callVoidRTS "tim_start" []
    fun <- lookupGlobalOperand name
    IR.call fun []
    IR.ret (IR.int32 0)

rtsFunctions :: [(Name, [LLVM.Type], LLVM.Type)]
rtsFunctions = [
    ("tim_start",                  [],                      voidType),
    ("tim_take",                   [longType, longType],    voidType),
    ("tim_push_argument_argument", [longType],              voidType),
    ("tim_push_argument_int",      [intVType],              voidType),
    ("tim_push_argument_double",   [doubleVType],           voidType),
    ("tim_push_argument_string",   [stringVType],           voidType),
    ("tim_push_argument_bool",     [boolVType],             voidType),
    ("tim_push_argument_unit",     [unitVType],             voidType),
    ("tim_push_argument_label",    [funPtrType],            voidType),
    ("tim_push_argument_data",     [longType],              voidType),
    ("tim_push_value_int",         [intVType],              voidType),
    ("tim_push_value_double",      [doubleVType],           voidType),
    ("tim_push_value_string",      [stringVType],           voidType),
    ("tim_push_value_bool",        [boolVType],             voidType),
    ("tim_push_value_unit",        [unitVType],             voidType),
    ("tim_push_value_data",        [tagVType],              voidType),
    ("tim_pop_value_int",          [],                      ptrType intVType),
    ("tim_pop_value_double",       [],                      ptrType doubleVType),
    ("tim_pop_value_string",       [],                      ptrType stringVType),
    ("tim_pop_value_bool",         [],                      ptrType boolVType),
    ("tim_pop_value_unit",         [],                      ptrType unitVType),
    ("tim_pop_value_data",         [],                      ptrType tagVType),
    ("tim_enter_argument",         [longType],              voidType),
    ("tim_enter_int",              [intVType],              voidType),
    ("tim_enter_double",           [doubleVType],           voidType),
    ("tim_enter_string",           [stringVType],           voidType),
    ("tim_enter_bool",             [boolVType],             voidType),
    ("tim_enter_unit",             [unitVType],             voidType),
    ("tim_enter_label",            [funPtrType],            voidType),
    ("tim_enter_data",             [longType],              voidType),
    ("tim_move_argument",          [longType, longType],    voidType),
    ("tim_move_int",               [longType, intVType],    voidType),
    ("tim_move_double",            [longType, doubleVType], voidType),
    ("tim_move_string",            [longType, stringVType], voidType),
    ("tim_move_bool",              [longType, boolVType],   voidType),
    ("tim_move_unit",              [longType, unitVType],   voidType),
    ("tim_move_label",             [longType, funPtrType],  voidType),
    ("tim_move_data",              [longType, longType],    voidType),
    ("tim_marker_push",            [longType],              voidType),
    ("tim_markers_update",         [longType],              voidType),
    ("tim_return",                 [],                      voidType),
    ("tim_data",                   [tagVType, funPtrType],  voidType),
    ("tim_switch_error",           [tagVType],              voidType)
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
  forM_ (codeStoreBlockNames store) $ \name -> do
    let mangled = mkBlockName name
    let op = mkGlobalOperand funPtrType mangled
    registerGlobalOperand name op

-- Code stores
emitCodeStore :: MonadLLVM m => CodeStore -> m ()
emitCodeStore store = do
  whenVerbose $ debug ("Emitting code store " <> prettyPrint (store_name store))
  forM_ (toList (store_blocks store)) $ \(name, code) -> do
    emitCodeBlock name code

-- Code blocks
emitCodeBlock :: MonadLLVM m => Name -> CodeBlock -> m ()
emitCodeBlock name code = void $ do
  whenVerbose $ debug ("Emitting code block " <> prettyPrint name)
  let funName = mkBlockName name
  IR.function funName [] voidType $ \[] -> do
    mapM_ emitInstr (toList code)

-- Instructions
emitInstr :: (MonadIRBuilder m, MonadLLVM m) => Instr -> m ()
emitInstr instr = do
  whenVerbose $ dump "Emitting instruction" instr
  case instr of
    -- Take
    TakeArgI t n -> do
      callVoidRTS "tim_take" [mkLong t, mkLong n]
    -- Push arguments
    PushArgI (ArgM n) -> do
      callVoidRTS "tim_push_argument_argument" [mkLong n]
    PushArgI (ValueM (IntV n)) -> do
      callVoidRTS "tim_push_argument_int" [mkIntV n]
    PushArgI (ValueM (DoubleV n)) -> do
      callVoidRTS "tim_push_argument_double" [mkDoubleV n]
    PushArgI (ValueM (StringV s)) -> do
      string <- registerString s
      callVoidRTS "tim_push_argument_string" [string]
    PushArgI (ValueM (BoolV b)) -> do
      callVoidRTS "tim_push_argument_bool" [mkBoolV b]
    PushArgI (ValueM (UnitV u)) -> do
      callVoidRTS "tim_push_argument_unit" [mkUnitV u]
    PushArgI (ValueM value) -> do
      throwInternalError ("emitInstr: impossible! trying to push argument " <> prettyPrint value)
    PushArgI (LabelM name) -> do
      fun <- lookupGlobalOperand name
      callVoidRTS "tim_push_argument_label" [fun]
    PushArgI (DataM field) -> do
      callVoidRTS "tim_push_argument_data" [mkLong field]
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
    PushValueI (InlineM (BoolV b)) -> do
      callVoidRTS "tim_push_value_bool" [mkBoolV b]
    PushValueI (InlineM (UnitV u)) -> do
      callVoidRTS "tim_push_value_unit" [mkUnitV u]
    PushValueI (InlineM value) -> do
      throwInternalError ("emitInstr: impossible! trying to push value " <> prettyPrint value)
    -- Markers
    PushMarkerI offset -> do
      callVoidRTS "tim_marker_push" [mkLong offset]
    UpdateMarkersI n -> do
      callVoidRTS "tim_markers_update" [mkLong n]
    -- Enter
    EnterI (ArgM n) -> do
      callVoidRTS "tim_enter_argument" [mkLong n]
    EnterI (ValueM (IntV n)) -> do
      callVoidRTS "tim_enter_value_int" [mkIntV n]
    EnterI (ValueM (DoubleV n)) -> do
      callVoidRTS "tim_enter_value_double" [mkDoubleV n]
    EnterI (ValueM (StringV s)) -> do
      string <- registerString s
      callVoidRTS "tim_enter_value_string" [string]
    EnterI (ValueM (BoolV b)) -> do
      callVoidRTS "tim_enter_value_bool" [mkBoolV b]
    EnterI (ValueM (UnitV u)) -> do
      callVoidRTS "tim_enter_value_unit" [mkUnitV u]
    EnterI (ValueM value) -> do
      throwInternalError ("emitInstr: impossible! trying to enter " <> prettyPrint value)
    EnterI (LabelM name) -> do
      fun <- lookupGlobalOperand name
      callVoidRTS "tim_enter_label" [fun]
    EnterI (DataM field) -> do
      callVoidRTS "tim_enter_data" [mkLong field]
    -- Move arguments
    MoveI slot (ArgM n) -> do
      callVoidRTS "tim_move_argument" [mkLong slot, mkLong n]
    MoveI slot (ValueM (IntV n)) -> do
      callVoidRTS "tim_move_int" [mkLong slot, mkIntV n]
    MoveI slot (ValueM (DoubleV n)) -> do
      callVoidRTS "tim_move_double" [mkLong slot, mkDoubleV n]
    MoveI slot (ValueM (StringV s)) -> do
      string <- registerString s
      callVoidRTS "tim_move_string" [mkLong slot, string]
    MoveI slot (ValueM (BoolV b)) -> do
      callVoidRTS "tim_move_bool" [mkLong slot, mkBoolV b]
    MoveI slot (ValueM (UnitV u)) -> do
      callVoidRTS "tim_move_unit" [mkLong slot, mkUnitV u]
    MoveI _ (ValueM value) -> do
      throwInternalError ("emitInstr: impossible! trying to move " <> prettyPrint value)
    MoveI slot (LabelM name) -> do
      fun <- lookupGlobalOperand name
      callVoidRTS "tim_move_label" [mkLong slot, fun]
    MoveI slot (DataM field) -> do
      callVoidRTS "tim_move_data" [mkLong slot, mkLong field]
    -- Return
    ReturnI -> do
      callVoidRTS "tim_return" []
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
        BoolT -> do
          ptr <- callRTS "tim_pop_value_bool" []
          IR.load ptr 0
        UnitT -> do
          ptr <- callRTS "tim_pop_value_unit" []
          IR.load ptr 0
        ty -> do
          throwInternalError ("emitInstr: impossible! trying to pop a value of type " <> prettyPrint ty)
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
        BoolT -> do
          callVoidRTS "tim_push_value_bool" [res]
        UnitT -> do
          callVoidRTS "tim_push_value_unit" [res]
        ty -> do
          throwInternalError ("emitInstr: impossible! trying to push a value of type " <> prettyPrint ty)
    -- Returning a data constructor
    DataI tag -> do
      conCode <- registerConstructorCode tag
      callVoidRTS "tim_data" [mkTag tag, conCode]
    -- Switch statements
    SwitchI alts def -> mdo
      IR.block `named` "switch.entry"
      -- Pop and load the tag from the value stack
      scrutTagPtr <- callRTS "tim_pop_value_data" []
      scrutTag <- IR.load scrutTagPtr 0
      -- Jump to the alternative corresponding to the tag we just popped
      IR.switch scrutTag defaultAlt altsTable
      -- Each case alternative simply calls its corresponding function
      altsTable <- forM (Map.toList alts) $ \(altTag, altLabel) -> do
        altBlock <- IR.block `named` ("switch." <> fromName altLabel)
        altCode <- lookupGlobalOperand altLabel
        IR.call altCode []
        return (mkTagConstant altTag, altBlock)
      -- The default alternative simply calls the error handler
      defaultAlt <- IR.block `named` "switch.default"
      case def of
        Nothing -> do
          callVoidRTS "tim_switch_error" [scrutTag]
        Just defLabel -> void $ do
          defCode <- lookupGlobalOperand defLabel
          IR.call defCode []
    -- Conditional instructions
    CondI thLabel elLabel -> void $ mdo
      IR.block `named` "cond.entry"
      -- Pop and load the condition value
      condPtr <- callRTS "tim_pop_value_bool" []
      cond <- IR.load condPtr 0
      -- Jump to the appropriate branch
      IR.condBr cond thBlock elBlock
      -- The code for each branch simply calls the corresponding function
      thBlock <- IR.block `named` "cond.then"
      thCode <- lookupGlobalOperand thLabel
      IR.call thCode []
      elBlock <- IR.block `named` "cond.else"
      elCode <- lookupGlobalOperand elLabel
      IR.call elCode []

----------------------------------------
-- Low-level utilities
----------------------------------------

-- Name manipulation

mkGlobalStringName :: Int -> LLVM.Name
mkGlobalStringName n = LLVM.mkName ("__string_" <> show n <> "__")

mkBlockName :: Name -> LLVM.Name
mkBlockName name = LLVM.mkName ("__block_" <> fromName name <> "__")

mkConCodeName :: Tag -> LLVM.Name
mkConCodeName tag = LLVM.mkName ("__con_" <> show tag <> "_code__")

-- Operands

mkGlobalOperand :: LLVM.Type -> LLVM.Name -> LLVM.Operand
mkGlobalOperand ty name = LLVM.ConstantOperand (Constant.GlobalReference ty name)

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

mkBoolV :: Bool -> LLVM.Operand
mkBoolV b = IR.bit (if b then 1 else 0)

mkUnitV :: Int -> LLVM.Operand
mkUnitV n = IR.int64 (fromIntegral n)

mkTag :: Int -> LLVM.Operand
mkTag n = IR.int64 (fromIntegral n)

mkTagConstant :: Integral a => a -> Constant.Constant
mkTagConstant n = Constant.Int 64 (fromIntegral n)

----------------------------------------
-- LLVM types
----------------------------------------

-- The type of a compiled supercombinator
-- In C: void (*f())
funPtrType :: LLVM.Type
funPtrType = ptrType (LLVM.FunctionType voidType [] False)

longType :: LLVM.Type
longType = LLVM.i64

voidType :: LLVM.Type
voidType = LLVM.void

ptrType :: LLVM.Type -> LLVM.Type
ptrType = LLVM.ptr

----------------------------------------
-- Value types

valueType :: Type -> LLVM.Type
valueType IntT    = intVType
valueType DoubleT = doubleVType
valueType StringT = stringVType
valueType BoolT   = boolVType
valueType UnitT   = unitVType
valueType TagT    = tagVType

-- NOTE: the ones below are platform dependent!

intVType :: LLVM.Type
intVType = LLVM.i64

doubleVType :: LLVM.Type
doubleVType = LLVM.double

stringVType :: LLVM.Type
stringVType = LLVM.ptr LLVM.i8

boolVType :: LLVM.Type
boolVType = LLVM.i1

unitVType :: LLVM.Type
unitVType = LLVM.i64

tagVType :: LLVM.Type
tagVType = LLVM.i64
