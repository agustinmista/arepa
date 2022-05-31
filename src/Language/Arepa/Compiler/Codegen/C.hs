module Language.Arepa.Compiler.Codegen.C
  ( CModule(..)
  , emitC
  , renderC
  ) where

import System.FilePath

import GHC.Exts

import Control.Monad.Extra
import Control.Monad.State

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Data.Loc

import Data.Maybe

import Data.Map (Map)
import Data.Map qualified as Map

import Language.C qualified as C
import Language.C.Quote.GCC

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad
import Language.Arepa.Compiler.IO
import Language.TIM

----------------------------------------
-- Code generation
----------------------------------------

data CModule = CModule {
  cm_name :: Name,
  cm_h_file :: [C.Definition],
  cm_c_file :: [C.Definition]
}

emitC :: MonadArepa m => CodeStore -> m CModule
emitC store = do
  let name = fromName (store_name store)
  runC name $ do
    emitRTSIncludes
    emitUserIncludes
    emitRTS
    emitCodeStore store

renderC :: MonadArepa m => CModule -> m (Text, Text)
renderC cm = do
  let renderDefs defs = renderMainland (stack (punctuate line (ppr <$> defs)))
  let h_file = renderDefs (cm_h_file cm)
  let c_file = renderDefs (cm_c_file cm)
  return (h_file, c_file)

renderMainland :: Doc -> Text
renderMainland doc = Text.pack (pretty 120 doc)

----------------------------------------
-- Code generation internal state
----------------------------------------

data CodegenState = CodegenState {
  cg_cmodule :: CModule,
  cg_constrs :: Map Tag Name
}

emptyCodegenState :: String -> CodegenState
emptyCodegenState name = CodegenState {
  cg_cmodule = CModule (mkName name) mempty mempty,
  cg_constrs = mempty
}

----------------------------------------
-- Code generation monads
----------------------------------------

-- The C module builder monad
type C m a = StateT CodegenState m a

runC :: MonadArepa m => String -> C m a -> m CModule
runC name mb = cg_cmodule <$> execStateT mb (emptyCodegenState name)

----------------------------------------
-- Generating variable names

freshVarName :: MonadArepa m => C m Name
freshVarName = do
  name <- gets (cm_name . cg_cmodule)
  liftIO (mkUniqueName "x" name)

freshVarNames :: MonadArepa m => Int -> C m [Name]
freshVarNames n = do
  name <- gets (cm_name . cg_cmodule)
  replicateM n (liftIO (mkUniqueName "x" name))

----------------------------------------
-- Looking up stuff in the environment

lookupPrim :: MonadArepa m => Name -> C m Prim
lookupPrim name = do
  whenVerbose $ debug ("Looking up primitive operation " <> prettyPrint name)
  case Map.lookup name primitives of
    Nothing -> do
      throwInternalError ("lookupPrim: cannot find primitive operation " <> prettyPrint name)
    Just prim -> do
      whenVerbose $ dump ("Found primitive operation " <> prettyPrint name) (prim_arity prim, prim_type prim)
      return prim

----------------------------------------
-- Data constructors code

-- Register a data constructor code
-- If it is already registered, then return the corresponding identifier
registerConstructorCode :: MonadArepa m => Tag -> C m Name
registerConstructorCode tag = do
  whenVerbose $ debug ("Registering data constructor " <> prettyPrint tag)
  constrs <- gets cg_constrs
  case Map.lookup tag constrs of
    Just i -> do
      whenVerbose $ debug ("The data constructor already had a global id: " <> prettyPrint i)
      return i
    Nothing -> do
      let i = mkConCodeName tag
      emitToHFile [cunit| void $id:i(); |]
      emitToCFile [cunit| void $id:i() { tim_data($exp:(mkTagV tag), *$id:i); } |]
      modify' $ \st -> st { cg_constrs = Map.insert tag i constrs }
      whenVerbose $ debug ("Created a new global data constructor code: " <> prettyPrint i)
      return i

----------------------------------------
-- Emitting stuff

emitToCFile :: MonadArepa m => [C.Definition] -> C m ()
emitToCFile def = do
  whenVerbose $ dump "Emitting to .c" (prettyShow def)
  modify' $ \st ->
    let c_file = cm_c_file (cg_cmodule st) in
    let cg_cmodule' = (cg_cmodule st) { cm_c_file = c_file <> def } in
    st { cg_cmodule = cg_cmodule' }

emitToHFile :: MonadArepa m => [C.Definition] -> C m ()
emitToHFile def = do
  whenVerbose $ dump "Emitting to .h" (prettyShow def)
  modify' $ \st ->
    let h_file = cm_h_file (cg_cmodule st) in
    let cg_cmodule' = (cg_cmodule st) { cm_h_file = h_file <> def } in
    st { cg_cmodule = cg_cmodule' }

emitIncludeToHFile :: MonadArepa m => FilePath -> C m ()
emitIncludeToHFile header = do
  let pragma = "#include " <> show header
  emitToHFile [cunit| $esc:pragma |]

emitIncludeToCFile :: MonadArepa m => FilePath -> C m ()
emitIncludeToCFile header = do
  let pragma = "#include " <> show header
  emitToCFile [cunit| $esc:pragma |]

----------------------------------------
-- Emitting RTS code
----------------------------------------

emitRTSIncludes :: MonadArepa m => C m ()
emitRTSIncludes = do
  whenVerbose $ debug "Emitting RTS #include's"
  emitIncludeToHFile "tim.h"

emitRTS :: MonadArepa m => C m ()
emitRTS = do
  -- RTS main wrapper (only when necessary or forced)
  whenVerbose $ debug "Emitting RTS main()"
  output <- lookupCompilerOption optOutput
  forced <- hasEmitMainEnabled
  linkingDisabled <- hasLinkingDisabled
  when (isJust output || forced || not linkingDisabled) $ do
    entry <- lookupCompilerOption optEntryPoint
    emitMain entry

emitMain :: MonadArepa m => String -> C m ()
emitMain entry = do
  let funName = mkBlockName (mkName entry)
  emitToHFile [cunit| int main(); |]
  emitToCFile [cunit| int main() { tim_start(); $id:funName(); return 0; } |]

----------------------------------------
-- Emitting user code
----------------------------------------

emitUserIncludes :: MonadArepa m => C m ()
emitUserIncludes = do
  whenVerbose $ debug "Emitting user include directives"
  -- Emit an include for the target's own header file
  h_path <- compiledHPath
  emitIncludeToCFile (takeFileName h_path)
  -- Emit a header for each extra included file
  extraFiles <- lookupCompilerOption optInclude
  forM_ extraFiles $ \extraFile -> do
    let i_path = mkHFilePath extraFile
    emitIncludeToCFile i_path

emitCodeStore :: MonadArepa m => CodeStore -> C m ()
emitCodeStore store = do
  whenVerbose $ debug ("Emitting code store " <> prettyPrint (store_name store))
  forM_ (toList (store_blocks store)) $ \(name, code) -> do
    emitCodeBlock name code

emitCodeBlock :: MonadArepa m => Name -> CodeBlock -> C m ()
emitCodeBlock name code = do
  whenVerbose $ debug ("Emitting code block " <> prettyPrint name)
  let funName = mkBlockName name
  emitToHFile [cunit| void $id:funName(); |]
  funItems <- concatMapM emitInstr code
  emitToCFile [cunit| void $id:funName() { $items:funItems } |]

emitInstr :: MonadArepa m => Instr -> C m [C.BlockItem]
emitInstr instr = do
  whenVerbose $ dump "Emitting instruction" instr
  case instr of
    -- Take
    TakeArgI t n -> do
      return [citems| tim_take ($exp:(mkLong t), $exp:(mkLong n)); |]
    -- Push arguments
    PushArgI (ArgM n) -> do
      return [citems| tim_push_argument_argument($exp:(mkLong n)); |]
    PushArgI (ValueM (IntV n)) -> do
      return [citems| tim_push_argument_int($exp:(mkIntV n)); |]
    PushArgI (ValueM (DoubleV n)) -> do
      return [citems| tim_push_argument_double($exp:(mkDoubleV n)); |]
    PushArgI (ValueM (StringV s)) -> do
      return [citems| tim_push_argument_string($exp:(mkStringV s)); |]
    PushArgI (ValueM (BoolV b)) -> do
      return [citems| tim_push_argument_bool($exp:(mkBoolV b)); |]
    PushArgI (ValueM (UnitV u)) -> do
      return [citems| tim_push_argument_unit($exp:(mkUnitV u)); |]
    PushArgI (ValueM value) -> do
      throwInternalError ("emitInstr: impossible! trying to push argument " <> prettyPrint value)
    PushArgI (LabelM label) -> do
      let funName = mkBlockName label
      return [citems| tim_push_argument_label(*$id:funName); |]
    PushArgI (DataM field) -> do
      return [citems| tim_push_argument_data($exp:(mkLong field)); |]
    -- Push values
    PushValueI FramePtrM -> do
      throwInternalError "emitInstr: impossible! we never generate instructions masking the frame pointer"
    PushValueI (InlineM (IntV n)) -> do
      return [citems| tim_push_value_int($exp:(mkIntV n)); |]
    PushValueI (InlineM (DoubleV n)) -> do
      return [citems| tim_push_value_double($exp:(mkDoubleV n)); |]
    PushValueI (InlineM (StringV s)) -> do
      return [citems| tim_push_value_string($exp:(mkStringV s)); |]
    PushValueI (InlineM (BoolV b)) -> do
      return [citems| tim_push_value_bool($exp:(mkBoolV b)); |]
    PushValueI (InlineM (UnitV u)) -> do
      return [citems| tim_push_value_unit($exp:(mkUnitV u)); |]
    PushValueI (InlineM value) -> do
      throwInternalError ("emitInstr: impossible! trying to push value " <> prettyPrint value)
    -- Markers
    PushMarkerI offset -> do
      return [citems| tim_marker_push($exp:(mkLong offset)); |]
    UpdateMarkersI n -> do
      return [citems| tim_markers_update($exp:(mkLong n)); |]
    -- Enter
    EnterI (ArgM n) -> do
      return [citems| tim_enter_argument($exp:(mkLong n)); |]
    EnterI (ValueM (IntV n)) -> do
      return [citems| tim_enter_value_int($exp:(mkIntV n)); |]
    EnterI (ValueM (DoubleV n)) -> do
      return [citems| tim_enter_value_double($exp:(mkDoubleV n)); |]
    EnterI (ValueM (StringV s)) -> do
      return [citems| tim_enter_value_string($exp:(mkStringV s)); |]
    EnterI (ValueM (BoolV b)) -> do
      return [citems| tim_enter_value_bool($exp:(mkBoolV b)); |]
    EnterI (ValueM (UnitV u)) -> do
      return [citems| tim_enter_value_unit($exp:(mkUnitV u)); |]
    EnterI (ValueM value) -> do
      throwInternalError ("emitInstr: impossible! trying to enter " <> prettyPrint value)
    EnterI (LabelM label) -> do
      let funName = mkBlockName label
      return [citems| tim_enter_label($id:funName); |]
    EnterI (DataM field) -> do
      return [citems| tim_enter_data($exp:(mkLong field)); |]
    -- Move arguments
    MoveI slot (ArgM n) -> do
      return [citems| tim_move_argument($exp:(mkLong slot), $exp:(mkLong n)); |]
    MoveI slot (ValueM (IntV n)) -> do
      return [citems| tim_move_int($exp:(mkLong slot), $exp:(mkIntV n)); |]
    MoveI slot (ValueM (DoubleV n)) -> do
      return [citems| tim_move_double($exp:(mkLong slot), $exp:(mkDoubleV n)); |]
    MoveI slot (ValueM (StringV s)) -> do
      return [citems| tim_move_string($exp:(mkLong slot), $exp:(mkStringV s)); |]
    MoveI slot (ValueM (BoolV b)) -> do
      return [citems| tim_move_bool($exp:(mkLong slot), $exp:(mkBoolV b)); |]
    MoveI slot (ValueM (UnitV u)) -> do
      return [citems| tim_move_unit($exp:(mkLong slot), $exp:(mkUnitV u)); |]
    MoveI _ (ValueM value) -> do
      throwInternalError ("emitInstr: impossible! trying to move " <> prettyPrint value)
    MoveI slot (LabelM label) -> do
      let funName = mkBlockName label
      return [citems| tim_move_label($exp:(mkLong slot), *$id:funName); |]
    MoveI slot (DataM field) -> do
      return [citems| tim_move_data($exp:(mkLong slot), $exp:(mkLong field)); |]
    -- Return
    ReturnI -> do
      return [citems| tim_return(); |]
    -- -- Call
    CallI name -> do
      prim <- lookupPrim name
      let (argTypes, retType) = prim_type prim
      -- Create some variables for the function parameters
      vars <- freshVarNames (length argTypes)
      -- Pop the function parameters from the value stack
      popArgs <- forM (zip argTypes vars) $ \case
        (IntT, var) -> do
          return [citem| typename Int* $id:var = tim_pop_value_int(); |]
        (DoubleT, var) -> do
          return [citem| typename Double* $id:var = tim_pop_value_double(); |]
        (StringT, var) -> do
          return [citem| typename String* $id:var = tim_pop_value_string(); |]
        (BoolT, var) -> do
          return [citem| typename Bool* $id:var = tim_pop_value_bool(); |]
        (UnitT, var) -> do
          return [citem| typename Unit* $id:var = tim_pop_value_unit(); |]
        ty -> do
          throwInternalError ("emitInstr: impossible! trying to pop a value of type " <> prettyPrint ty)
      -- Create the function call by hand
      funCallExp <- do
        return (C.FnCall [cexp| $id:name |] (fmap (\var -> [cexp| *$id:var |]) vars) noLoc)
      -- Call the function
      callFun <- case retType of
        IntT -> do
          return [citems| typename Int res = $exp:(funCallExp); tim_push_value_int(res); |]
        DoubleT -> do
          return [citems| typename Double res = $exp:(funCallExp); tim_push_value_double(res); |]
        StringT -> do
          return [citems| typename String res = $exp:(funCallExp); tim_push_value_string(res); |]
        BoolT -> do
          return [citems| typename Bool res = $exp:(funCallExp); tim_push_value_bool(res); |]
        UnitT -> do
          return [citems| typename Unit res = $exp:(funCallExp); tim_push_value_unit(res); |]
        ty -> do
          throwInternalError ("emitInstr: impossible! trying to push a value of type " <> prettyPrint ty)
      return (popArgs <> callFun)
    -- Returning a data constructor
    DataI tag -> do
      con <- registerConstructorCode tag
      return [citems| tim_data($exp:(mkTagV tag), $id:con); |]
    -- Switch statements
    SwitchI alts def -> do
      -- Pop the constructor tag
      scrut <- freshVarName
      popScrutTag <- do
        return [citems| typename tag_t* $id:scrut = tim_pop_value_data(); |]
      -- Build the switch cases
      tagAlts <- concatForM (Map.toList alts) $ \(tag, label) -> do
        let funName = mkBlockName label
        return [citems| case $exp:(mkTagV tag): $id:funName(); break; |]
      defAlt <- case def of
        Nothing -> do
          return [citems| default: tim_switch_error(*$id:scrut); break; |]
        Just label -> do
          let funName = mkBlockName label
          return [citems| default: $id:funName(); break; |]
      -- Build the switch statement
      switchStmt <- do
        return [citems| switch (*$id:scrut) { $items:(tagAlts <> defAlt) } |]
      return (popScrutTag <> switchStmt)
    -- Conditional instructions
    CondI thLabel elLabel -> do
      -- Pop the boolean value
      scrut <- freshVarName
      popBool <- do
        return [citems| typename Bool* $id:scrut = tim_pop_value_bool(); |]
      -- Build the if statement
      ifStmt <- do
        let thFunName = mkBlockName thLabel
        let elFunName = mkBlockName elLabel
        return [citems| if (*$id:scrut) { $id:thFunName(); } else { $id:elFunName(); } |]
      return (popBool <> ifStmt)
    -- Freeze the value stack
    FreezeI -> do
      return [citems| tim_freeze(); |]
    -- Restore the value stack to the previous one
    RestoreI -> do
      return [citems| tim_restore(); |]

----------------------------------------
-- Low-level utilities
----------------------------------------

-- Name manipulation

mkBlockName :: Name -> Name
mkBlockName name = mkName ("__block_" <> fromName name <> "__")

mkConCodeName :: Tag -> Name
mkConCodeName tag = mkName ("__con_" <> show tag <> "_code__")

-- Creating literal values

mkLong :: Int -> C.Exp
mkLong n = [cexp| $lint:n |]

-- Arepa values

mkIntV :: Int -> C.Exp
mkIntV n = [cexp| $lint:n |]

mkDoubleV :: Double -> C.Exp
mkDoubleV n = [cexp| $double:n |]

mkStringV :: Text -> C.Exp
mkStringV s = [cexp| $string:(Text.unpack s) |]

mkBoolV :: Bool -> C.Exp
mkBoolV True  = [cexp| true  |]
mkBoolV False = [cexp| false |]

mkUnitV :: Int -> C.Exp
mkUnitV _ = [cexp| unit |]

mkTagV :: Int -> C.Exp
mkTagV n = [cexp| $lint:n |]
