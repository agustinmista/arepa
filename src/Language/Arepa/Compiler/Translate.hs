module Language.Arepa.Compiler.Translate
  ( translateModule
  , CodeStore(..)
  , encodeCodeStore
  , decodeCodeStore
  ) where

import Control.Monad.Extra
import Control.Monad.State

import Data.Either

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
  whenVerbose $ dump "Running in local environment with binds" binds
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
inNewCodeBlock prefix ma = do
  st <- get
  label <- liftIO (mkUniqueName (store_name (ts_store st)) prefix)
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
    -- Conditional expressions
    IfE cond th el -> do
      translateIf cond th el
    -- Case expressions
    CaseE scrut alts -> do
      translateCase scrut alts
    -- Sequential expressions
    SeqE e1 e2 -> do
      translateSeq e1 e2

----------------------------------------
-- Primitive function calls

-- This uses continuation passing style to build the chain of functions that
-- prepare the value stack to call the primitive operation.

-- NOTE: Primitive calls' arguments can safely reuse each other's frame slots
-- because we force their evaluation to happen sequentially.

translateCall :: MonadArepa m => Name -> [CoreExpr] -> Translate m CodeBlock
translateCall name args = do
  whenVerbose $ dump "Translating primitive call" (name, args)
  let isolatedTranslateArg arg cont = withIsolatedFrameSlots (translateValueStackOperand arg cont)
  let callCode = return [ CallI name, ReturnI ]
  (slots, cpsCode) <- chainAccumCPS (isolatedTranslateArg <$> args) callCode
  unless (null slots) $ do
    setFrameSlots (maximum slots)
  return cpsCode

----------------------------------------
-- Variables

translateVar :: MonadArepa m => Name -> Translate m CodeBlock
translateVar name = do
  whenVerbose $ dump "Translating variable" name
  mode <- lookupArgMode name
  return [ EnterI mode ]

----------------------------------------
-- Literals

translateLit :: MonadArepa m => Lit -> Translate m CodeBlock
translateLit lit = do
  whenVerbose $ dump "Translating literal" lit
  let value = litValue lit
  return [ PushValueI (InlineM value), ReturnI ]

----------------------------------------
-- Function applications

translateApp :: MonadArepa m => CoreExpr -> CoreExpr -> Translate m CodeBlock
translateApp fun op = do
  whenVerbose $ dump "Translating function application" (fun, op)
  opCode <- translateArgumentStackOperand op
  funCode <- translateExpr fun
  return (opCode <> funCode)

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
-- Let expressions

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
  rhsMode <- translateUpdatableExpr slot (snd bind)
  return [ MoveI slot rhsMode ]

----------------------------------------
-- Conditional expressions

translateIf :: MonadArepa m => CoreExpr -> CoreExpr -> CoreExpr -> Translate m CodeBlock
translateIf cond th el = do
  whenVerbose $ dump "Translating if expression" (cond, th, el)
  condLabel <- inNewCodeBlock "cond" $ do
    (thSlots, thLabel) <- withIsolatedFrameSlots $ do
      inNewCodeBlock "then" (translateExpr th)
    (elSlots, elLabel) <- withIsolatedFrameSlots $ do
      inNewCodeBlock "else" (translateExpr el)
    let slots = max thSlots elSlots
    setFrameSlots slots
    return [ CondI thLabel elLabel ]
  translateValueStackOperand cond [ EnterI (LabelM condLabel) ]

----------------------------------------
-- Case expressions

translateCase :: MonadArepa m => CoreExpr -> [CoreAlt] -> Translate m CodeBlock
translateCase scrut alts = do
  whenVerbose $ dump "Translating case expression" (scrut, alts)
  -- Create the code for the switch
  caseLabel <- inNewCodeBlock "case" $ do
    -- Translate the case alternatives each one in an isolated environment
    let isolatedTranslateAlt alt = withIsolatedFrameSlots (translateAlt alt)
    (slots, altBranches) <- unzip <$> mapM isolatedTranslateAlt alts
    unless (null slots) $ do
      setFrameSlots (maximum slots)
    -- Find if we have a default branch and, if so, use it as the switch's default branch
    case partitionEithers altBranches of
      (consBranches, [defLabel]) -> do
        return [ SwitchI (Map.fromList consBranches) (Just defLabel) ]
      (consBranches, _) -> do
        return [ SwitchI (Map.fromList consBranches) Nothing ]
  -- Translate the scrutinee
  scrutCode <- translateExpr scrut
  -- Push the switch continuation and run the code for the scrutinee
  return ([ PushArgI (LabelM caseLabel) ] <> scrutCode)

translateAlt :: MonadArepa m => CoreAlt -> Translate m (Either (Int, Label) Label)
translateAlt alt = do
  case alt of
    ConA con vars body -> do
      Left <$> translateConA con vars body
    DefA var body -> do
      Right <$> translateDefA var body

translateConA :: MonadArepa m => Con -> [Name] -> CoreExpr -> Translate m (Int, Label)
translateConA con vars body = do
  whenVerbose $ dump "Translating constructor case alternative" (con, vars, body)
  -- Old slots are needed to calculate the move insructions
  oldSlots <- getFrameSlots
  -- Allocate some frame slots for the constructor arguments
  slots <- reserveFrameSlots (length vars)
  -- Translate the alternative RHS in an enviroment extended with the
  -- constructor case variables
  let rhsEnv = zip vars (ArgM <$> slots)
  label <- inLocalEnvWith rhsEnv $ do
    inNewCodeBlock "con" $ do
      let moveCode = [ MoveI slot (DataM (slot-oldSlots)) | slot <- slots ]
      bodyCode <- translateExpr body
      return (moveCode <> bodyCode)
  return (con_tag con, label)

translateDefA :: MonadArepa m => Name -> CoreExpr -> Translate m Label
translateDefA var body = do
  whenVerbose $ dump "Translating default case alternative" (var, body)
  -- Old slots are needed to calculate the move insructions
  oldSlots <- getFrameSlots
  -- Allocate one frame slot for the evaluated value
  [slot] <- reserveFrameSlots 1
  -- Translate the alternative RHS in an enviroment extended with the value
  let rhsEnv = [(var, ArgM slot)]
  inLocalEnvWith rhsEnv $ do
    inNewCodeBlock "def" $ do
      let moveCode = [ MoveI slot (ArgM (slot-oldSlots)) ]
      bodyCode <- translateExpr body
      return (moveCode <> bodyCode)

----------------------------------------
-- Sequential expressions

translateSeq :: MonadArepa m => CoreExpr -> CoreExpr -> Translate m CodeBlock
translateSeq e1 e2 = do
  whenVerbose $ dump "Translating sequential expression" (e1, e2)
  -- Translate the second expression into a continuation label to jump to after
  -- evaluating the first expression
  e2Label <- inNewCodeBlock "seq" $ do
    e2Code <- translateExpr e2
    return ([ RestoreI ] <> e2Code)
  -- Translate the expression we will evaluate to WHNF
  e1Code <- translateExpr e1
  return ([ PushArgI (LabelM e2Label), FreezeI ] <> e1Code)

----------------------------------------
-- Helpers
----------------------------------------

-- Translate an expression into the code that pushes its closure to the argument stack.
translateArgumentStackOperand :: MonadArepa m => CoreExpr -> Translate m CodeBlock
translateArgumentStackOperand expr = do
  whenVerbose $ dump "Translating expression to be pushed to the argument stack" expr
  case expr of
    NonPrimVarE name -> do
      mode <- lookupArgMode name
      return [ PushArgI mode ]
    LitE lit -> do
      let value = litValue lit
      return [ PushArgI (ValueM value) ]
    _ -> do
      offset <- getFrameSlots
      setFrameSlots (offset + 1)
      mode <- translateUpdatableExpr offset expr
      return [ MoveI offset mode, PushArgI mode ]

-- Translate an expression into the code that pushes its (evaluated) result into
-- the value stack and proceeds to call the continuation that expects it.
translateValueStackOperand :: MonadArepa m => CoreExpr -> CodeBlock -> Translate m CodeBlock
translateValueStackOperand arg cont = do
  whenVerbose $ dump "Translating expression to be pushed to the value stack" (arg, cont)
  case arg of
    LitE lit -> do
      let value = litValue lit
      return ([ PushValueI (InlineM value) ] <> cont)
    expr -> do
      label <- inNewCodeBlock "cont" (return cont)
      code <- translateExpr expr
      return ([ PushArgI (LabelM label) ] <> code)

-- Translate an expression into the address mode of a self updating closure.
translateUpdatableExpr :: MonadArepa m => Offset -> CoreExpr -> Translate m ArgMode
translateUpdatableExpr offset expr = do
  whenVerbose $ dump "Translating argument mode of a self-updating expression closure" expr
  case expr of
    NonPrimVarE name -> do
      lookupArgMode name
    LitE lit -> do
      let value = litValue lit
      return (ValueM value)
    _ -> do
      code <- translateExpr expr
      label <- inNewCodeBlock "expr" $ do
        return ([ PushMarkerI offset ] <> code)
      return (LabelM label)

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
    BoolL   s -> BoolV s
    UnitL     -> UnitV 0

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
