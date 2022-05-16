module Language.Arepa.Compiler.Translate
  ( translateModule
  , CodeStore(..)
  ) where

import Control.Monad.Extra
import Control.Monad.State

import Data.Map (Map)
import Data.Map qualified as Map

import Language.Arepa.Syntax
import Language.Arepa.Prim
import Language.Arepa.Compiler.Monad

import Language.TIM

----------------------------------------
-- Arepa to TIM translation
----------------------------------------

-- Translate a core module into TIM code
translateModule :: MonadArepa m => CoreModule -> m CodeStore
translateModule m = do
  let name = modName m
  let globals = declName <$> modDecls m
  whenVerbose $ debug ("Translating module " <> prettyPrint name)
  runTranslate name globals $ do
    mapM_ translateDecl (modDecls m)

----------------------------------------
-- Internal translation state

data TranslateState = TranslateState {
  ts_store :: CodeStore,          -- ^ The code store we build under the hood
  ts_env   :: Map Name ArgMode,   -- ^ The current stuff in scope
  ts_slots :: Int                 -- ^ The amount of extra frame slots needed for letrecs
}

initialTranslateState :: Name -> [Name] -> TranslateState
initialTranslateState name globals = TranslateState {
  ts_store = emptyCodeStore name,
  ts_env = Map.fromList [ (global, LabelM global) | global <- globals ],
  ts_slots = 0
}

----------------------------------------
-- Translation monad

-- A translator transformer that runs on top of the compiler monad

type Translate m a = StateT TranslateState m a

runTranslate :: MonadArepa m => Name -> [Name] -> Translate m a -> m CodeStore
runTranslate name globals ma = ts_store <$> execStateT ma (initialTranslateState name globals)

----------------------------------------
-- Monad operations

-- Lookup for the addressing mode of an identifier in the environment
-- NOTE: this assumes that the arg mode is always a label if the identifier is
-- not defined here
lookupArgMode :: MonadArepa m => Name -> Translate m ArgMode
lookupArgMode name = do
  whenVerbose $ debug ("Looking up argument mode of " <> prettyPrint name)
  env <- gets ts_env
  case Map.lookup name env of
    Nothing -> do
      whenVerbose $ debug ("Argument mode for " <> prettyPrint name <> " is missing, assumming it is an extern")
      return (LabelM name)
    Just mode -> do
      whenVerbose $ debug ("Found argument mode for " <> prettyPrint name <> ": " <> prettyPrint mode)
      return mode

-- Run the translation inside a local environment
inLocalEnvWith :: MonadArepa m => [(Name, ArgMode)] -> Translate m a -> Translate m a
inLocalEnvWith binds ma = do
  whenVerbose $ dump "Extending the current environment with" binds
  st <- get
  put (st { ts_env = foldr (uncurry Map.insert) (ts_env st) binds })
  a <- ma
  modify' $ \st' -> st' { ts_env = ts_env st }
  whenVerbose $ dump "Restored the previous environment to" (Map.toList (ts_env st))
  return a

-- Insert a code bind in the internal code store
saveCodeBlock :: MonadArepa m => Name -> CodeBlock -> Translate m ()
saveCodeBlock name code = do
  whenVerbose $ dump ("Saving code block " <> prettyPrint name) code
  modify $ \st -> st { ts_store = insertCodeStore name code (ts_store st) }

-- Run the translator inside a new code block with a given prefix
inNewCodeBlock :: MonadArepa m => Name -> Translate m CodeBlock -> Translate m Name
inNewCodeBlock name ma = do
  label <- liftIO (mkUniqueName name)
  whenVerbose $ debug ("Translating code into new code block " <> prettyPrint label)
  code <- ma
  saveCodeBlock label code
  return label

-- Return the acummulated number of frame slots
getFrameSlots :: MonadArepa m => Translate m Int
getFrameSlots = gets ts_slots

-- Set the frame slots counter
setFrameSlots :: MonadArepa m => Int -> Translate m ()
setFrameSlots slots = do
  whenVerbose $ debug "Resetting frame slots counter"
  modify' $ \st -> st { ts_slots = slots }

-- Reserve some extra frame slots
reserveFrameSlots :: MonadArepa m => Int -> Translate m [Int]
reserveFrameSlots n = do
  slots <- getFrameSlots
  let reserved = take n [ slots .. ]
  whenVerbose $ dump "Reserving frame slots" reserved
  setFrameSlots (slots + n)
  return reserved

-- Run a computation isolating the new frame slots it reserves
withIsolatedFrameSlots :: MonadArepa m => Translate m a -> Translate m (Int, a)
withIsolatedFrameSlots ma = do
  slots <- getFrameSlots
  a <- ma
  slots' <- getFrameSlots
  setFrameSlots slots
  return (slots', a)

----------------------------------------
-- Translation operations
----------------------------------------

----------------------------------------
-- Declarations

translateDecl :: MonadArepa m => CoreDecl -> Translate m ()
translateDecl decl = do
  whenVerbose $ dump "Translating declaration" decl
  let name = declName decl
  let args = declArgs decl
  let extEnv = zip args (ArgM <$> [0..])
  setFrameSlots (length args)
  bodyCode <- inLocalEnvWith extEnv $ do
    translateExpr (declBody decl)
  totalSlots <- getFrameSlots
  whenVerbose $ debug ("Total frame slots for " <> prettyPrint name <> ": " <> prettyPrint totalSlots)
  let n = length args
  let updateCode | n > 0     = [ UpdateMarkersI n ]
                 | otherwise = []
  let takeCode = [ TakeArgI totalSlots n ]
  saveCodeBlock name (updateCode <> takeCode <> bodyCode)

----------------------------------------
-- Expressions

translateExpr :: MonadArepa m => CoreExpr -> Translate m CodeBlock
translateExpr expr = do
  case expr of
    -- Fully saturated primitive function calls
    -- NOTE: must be at top since this pattern overlaps VarE and AppE
    CallE name args -> do
      translateCall name args
    -- Variables
    VarE name -> do
      translateVar name
    -- Literal values
    LitE lit -> do
      translateLit lit
    -- Data constructors
    ConE con -> do
      translateCon con
    -- Function application
    AppE fun op -> do
      translateApp fun op
    -- Lambda functions should be gone by now
    LamE _ _ -> do
      throwInternalError "translateInstr: impossible! lambda expressions should never appear here"
    -- Let bindings
    LetE isRec binds body -> do
      translateLet isRec binds body
    -- Case expressions
    CaseE scrut alts -> do
      translateCase scrut alts

----------------------------------------
-- Primitive function calls

-- This uses continuation passing style to build the chain of functions that
-- prepare the value stack to call the primitive operation.

translateCall :: MonadArepa m => Name -> [CoreExpr] -> Translate m CodeBlock
translateCall name args = do
  whenVerbose $ dump "Translating primitive call" (name, args)
  let isolatedTranslateArg arg cont = withIsolatedFrameSlots (translateCallArg arg cont)
  let callCode = return [ CallI name, ReturnI ]
  (slots, cpsCode) <- chainAccumCPS (isolatedTranslateArg <$> args) callCode
  unless (null slots) $ do
    setFrameSlots (maximum slots)
  return cpsCode

-- Primitive calls' arguments can safely reuse each other's frame slots because
-- we force their evaluation to happen sequentially.

translateCallArg :: MonadArepa m => CoreExpr -> CodeBlock -> Translate m CodeBlock
translateCallArg arg cont = do
  whenVerbose $ dump "Translating call argument" arg
  case arg of
    LitE lit -> do
      mode <- translateValueMode lit
      return ([ PushValueI mode ] <> cont)
    expr -> do
      label <- inNewCodeBlock "cont" $ do
        return cont
      code <- translateExpr expr
      return ([ PushArgI (LabelM label) ] <> code)

----------------------------------------
-- Variables

translateVar :: MonadArepa m => Name -> Translate m CodeBlock
translateVar name = do
  whenVerbose $ dump "Translating variable" name
  mode <- translateArgMode 0 (VarE name)
  return [ EnterI mode ]

----------------------------------------
-- Literals

translateLit :: MonadArepa m => Lit -> Translate m CodeBlock
translateLit lit = do
  whenVerbose $ dump "Translating literal" lit
  mode <- translateValueMode lit
  return [ PushValueI mode, ReturnI ]

----------------------------------------
-- Function applications

translateApp :: MonadArepa m => CoreExpr -> CoreExpr -> Translate m CodeBlock
translateApp fun op = do
  whenVerbose $ dump "Translating function application" (fun, op)
  opCode <- translateOperand op
  funCode <- translateExpr fun
  return (opCode <> funCode)

translateOperand :: MonadArepa m => CoreExpr -> Translate m CodeBlock
translateOperand expr = do
  whenVerbose $ dump "Translating operand expression" expr
  case expr of
    VarE {} -> do
      mode <- translateArgMode 0 expr
      return [ PushArgI mode ]
    LitE {} -> do
      mode <- translateArgMode 0 expr
      return [ PushArgI mode ]
    _ -> do
      slots  <- getFrameSlots
      setFrameSlots (slots + 1)
      mode  <- translateArgMode slots expr
      return [ MoveI slots mode, PushArgI mode ]

----------------------------------------
-- Data constructors

translateCon :: MonadArepa m => Con -> Translate m CodeBlock
translateCon con = do
  whenVerbose $ dump "Translating data constructor" con
  let tag = con_tag con
  let arity = con_arity con
  let updateCode | arity > 0 = [ UpdateMarkersI arity ]
                 | otherwise = []
  return (updateCode <> [ TakeArgI arity arity, DataI tag ])

----------------------------------------
-- Let binds

translateLet :: MonadArepa m => Bool -> [(Name, CoreExpr)] -> CoreExpr -> Translate m CodeBlock
translateLet isRec binds body = do
  whenVerbose $ dump "Translating let expression" (isRec, binds, body)
  -- The local slots used by this let expression
  slots <- reserveFrameSlots (length binds)
  let indexedBinds = zip binds slots
  -- Compute the extended environment used for letrec
  -- (creates an indirection closure for each local bind)
  bindsEnv <- forM indexedBinds $ \(bind, slot) -> do
    let label = fst bind
    saveCodeBlock label [ EnterI (ArgM slot) ]
    return (label, LabelM label)
  -- The environment used for the RHS of a let bind
  let rhsEnv | isRec     = bindsEnv
             | otherwise = []
  -- Translate the let right-hand sides
  bindsCode <- inLocalEnvWith rhsEnv $ do
    traverse (uncurry translateLetBind) indexedBinds
  -- Translate the body with the extended bind environment
  exprCode <- inLocalEnvWith bindsEnv $ do
    translateExpr body
  return (mconcat bindsCode <> exprCode)

translateLetBind :: MonadArepa m => (Name, CoreExpr) -> Int -> Translate m CodeBlock
translateLetBind bind slot = do
  whenVerbose $ dump "Translating let bind" bind
  rhsMode <- translateArgMode slot (snd bind)
  return [ MoveI slot rhsMode ]

----------------------------------------
-- Case expressions

translateCase :: MonadArepa m => CoreExpr -> [CoreAlt] -> Translate m CodeBlock
translateCase scrut alts = do
  whenVerbose $ dump "Translate case expression" (scrut, alts)
  -- Create the code for the switch
  label <- inNewCodeBlock "switch" $ do
    -- Translate the case alternatives each one in an isolated environment
    let isolatedTranslateAlt alt = withIsolatedFrameSlots (translateAlt alt)
    (slots, altsCode) <- unzip <$> mapM isolatedTranslateAlt alts
    unless (null slots) $ do
      setFrameSlots (maximum slots)
    -- Build the tags to codeblocks mappings
    let altsMap = Map.fromList altsCode
    return [ SwitchI altsMap ]
  -- Translate the scrutinee
  scrutCode <- translateExpr scrut
  -- Push the switch continuation and run the code for the scrutinee
  return ([ PushArgI (LabelM label) ] <> scrutCode)

translateAlt :: MonadArepa m => CoreAlt -> Translate m (Int, Label)
translateAlt alt = do
  whenVerbose $ dump "Translate case alternative" alt
  case alt of
    Alt con vars expr -> do
      -- Old slots are needed to calculate the move insructions
      oldSlots <- getFrameSlots
      -- Allocate some frame slots for the constructor arguments
      slots <- reserveFrameSlots (length vars)
      -- Translate the alternative RHS in an enviroment extended with the
      -- constructor case variables
      let rhsEnv = zip vars (ArgM <$> slots)
      label <- inLocalEnvWith rhsEnv $ do
        inNewCodeBlock "alt" $ do
          let moveCode = CodeBlock [ MoveI slot (DataM (slot-oldSlots)) | slot <- slots ]
          exprCode <- translateExpr expr
          return (moveCode <> exprCode)
      return (con_tag con, label)

----------------------------------------
-- Addressing modes
----------------------------------------

translateArgMode :: MonadArepa m => Offset -> CoreExpr -> Translate m ArgMode
translateArgMode offset expr = do
  whenVerbose $ dump "Translating address mode of expression" expr
  case expr of
    VarE name -> do
      lookupArgMode name
    LitE lit -> do
      let value = litValue lit
      return (ValueM value)
    _ -> do
      code <- translateExpr expr
      label <- inNewCodeBlock "arg" $ do
        return ([ PushMarkerI offset ] <> code)
      return (LabelM label)

translateValueMode :: MonadArepa m => Lit -> Translate m ValueMode
translateValueMode lit = do
  whenVerbose $ dump "Translating value mode of literal" lit
  let value = litValue lit
  return (InlineM value)

----------------------------------------
-- Utilities
----------------------------------------

-- Values associated to literals

litValue :: Lit -> Value
litValue lit = do
  case lit of
    IntL    n -> IntV n
    DoubleL n -> DoubleV n
    StringL s -> StringV s

-- Thread a list of CPS monadic computations into a single result with an
-- accumulator. This operation is right associative, meaning that the `a`
-- obtained from the `m a` input is passed to the last computation.

chainAccumCPS :: Monad m => [a -> m (b, a)] -> m a -> m ([b], a)
chainAccumCPS = go . reverse
  where
    go [] k = do
      a <- k
      return ([], a)
    go (m:ms) k = do
      (bs, a')  <- go ms k
      (b,  a'') <- m a'
      return (b : bs, a'')
