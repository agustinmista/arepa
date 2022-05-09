module Language.Arepa.Compiler.Translate
  ( translateModule
  ) where

import Control.Monad.Extra
import Control.Monad.State

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Text.Lazy (Text)

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad

import Language.TIM

----------------------------------------
-- Arepa to TIM translation
----------------------------------------

-- Translate a core module into TIM code
translateModule :: MonadArepa m => CoreModule -> m CodeStore
translateModule m = do
  let name = mod_name m
  let globals = declName <$> mod_decls m
  whenVerbose $ debug ("Translating module " <> prettyPrint name)
  runTranslate name globals $ do
    mapM_ translateDecl (mod_decls m)

----------------------------------------
-- Internal translation state

data TranslateState = TranslateState {
  ts_store :: CodeStore,          -- ^ The code store we build under the hood
  ts_env   :: Map Name ArgMode,   -- ^ The current stuff in scope
  ts_slots :: Int,                -- ^ The amount of extra frame slots needed for letrecs
  ts_fresh :: Int                 -- ^ A source of fresh names
}

initialTranslateState :: Name -> [Name] -> TranslateState
initialTranslateState name globals = TranslateState {
  ts_store = emptyCodeStore name,
  ts_env = Map.fromList [ (global, LabelM global) | global <- globals ],
  ts_slots = 0,
  ts_fresh = 0
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
withExtendedEnv :: MonadArepa m => [(Name, ArgMode)] -> Translate m a -> Translate m a
withExtendedEnv binds ma = do
  whenVerbose $ dump "Extending the current environment with" (prettyPrint binds)
  st <- get
  put (st { ts_env = foldr (uncurry Map.insert) (ts_env st) binds })
  a <- ma
  modify' $ \st' -> st' { ts_env = ts_env st }
  whenVerbose $ dump "Restored the previous environment to" (prettyPrint (Map.toList (ts_env st)))
  return a

-- Insert a code bind in the internal code store
saveCodeBlock :: MonadArepa m => Name -> CodeBlock -> Translate m ()
saveCodeBlock name code = do
  whenVerbose $ dump ("Saving code block " <> prettyPrint name) (prettyPrint code)
  modify $ \st -> st { ts_store = insertCodeStore name code (ts_store st) }

-- Return a fresh name with a given prefix
freshName :: MonadArepa m => Text -> Translate m Name
freshName prefix = state $ \st ->
  (mkNameWithNum prefix (ts_fresh st), st { ts_fresh = ts_fresh st + 1 })

-- Lookup a primitive operation by its name
lookupPrimOp :: MonadArepa m => Name -> Translate m Prim
lookupPrimOp name = do
  whenVerbose $ debug ("Looking up for primitive operation " <> prettyPrint name)
  case Map.lookup name primitives of
    Nothing -> do
      throwInternalError ("lookupPrimOp: primitive " <> prettyPrint name <> " is missing")
    Just prim -> do
      whenVerbose $ dump ("Found primitive operation " <> prettyPrint name) (prettyPrint (prim_arity prim, prim_type prim))
      return prim

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
reserveFrameSlots slots = do
  st <- get
  let reserved = take slots [ ts_slots st .. ]
  whenVerbose $ dump "Reserving frame slots" (prettyPrint reserved)
  put (st { ts_slots = ts_slots st + slots })
  return reserved

----------------------------------------
-- Translation operations
----------------------------------------

----------------------------------------
-- Declarations

translateDecl :: MonadArepa m => CoreDecl -> Translate m ()
translateDecl decl = do
  whenVerbose $ dump "Translating declaration" (prettyPrint decl)
  let args = declArgs decl
  let extEnv = zip args (ArgM <$> [0..])
  setFrameSlots (length args)
  bodyInstrs <- withExtendedEnv extEnv (translateExpr (declBody decl))
  totalSlots <- getFrameSlots
  whenVerbose $ debug ("Total frame slots " <> prettyPrint totalSlots)
  let takeArgs = [ TakeArgI totalSlots (length args) ]
  saveCodeBlock (declName decl) (takeArgs <> bodyInstrs)

----------------------------------------
-- Expressions

translateExpr :: MonadArepa m => CoreExpr -> Translate m CodeBlock
translateExpr expr = do
  whenVerbose $ dump "Translating expression" (prettyPrint expr)
  case expr of
    -- Fully saturated primitive function calls
    -- NOTE: must be at top since this pattern overlaps VarE and AppE
    CallE name args -> do
      translateCall name args
    -- Variables
    VarE name -> do
      mode <- translateArgMode (VarE name)
      return [ EnterI mode ]
    -- Literal values
    LitE lit -> do
      mode <- translateValueMode lit
      return [ PushValueI mode, ReturnI ]
    -- Data constructors
    ConE _con -> do
      notImplemented "translateExpr/ConE"
    -- Function application
    AppE e1 e2 -> do
      e1code <- translateExpr    e1
      mode   <- translateArgMode e2
      return ([ PushArgI mode ] <> e1code)
    -- Lambda functions should be gone by now
    LamE _var _body -> do
      notImplemented "translateExpr/LamE"
    -- Let bindings
    LetE isRec binds body -> do
      translateLet isRec binds body
    -- Case expressions
    CaseE _scrut _alts -> do
      notImplemented "translateExpr/CaseE"

----------------------------------------
-- Primitive function calls

-- This uses continuation passing style to build the chain of functions that
-- prepare the value stack to call the primitive operation.

translateCall :: MonadArepa m => Name -> [CoreExpr] -> Translate m CodeBlock
translateCall name args = do
  whenVerbose $ dump "Translating primitive call" (prettyPrint (name, args))
  prim <- lookupPrimOp name
  when (prim_arity prim /= length args) $ do
    throwInternalError ("translateCall: primitive " <> prettyPrint name <> " must take exactly " <> prettyPrint (prim_arity prim) <> " arguments")
  translateCallArgs args [ CallI name, ReturnI ]

translateCallArgs :: MonadArepa m => [CoreExpr] -> CodeBlock -> Translate m CodeBlock
translateCallArgs args cont = do
  case args of
    [] -> do
      return cont
    LitE lit : rest -> do
      mode <- translateValueMode lit
      translateCallArgs rest ([ PushValueI mode ] <> cont)
    expr : rest -> do
      label <- freshName "cont"
      saveCodeBlock label cont
      code <- translateExpr expr
      translateCallArgs rest ([ PushArgI (LabelM label) ] <> code)

----------------------------------------
-- Let binds

translateLet :: MonadArepa m => Bool -> [(Name, CoreExpr)] -> CoreExpr -> Translate m CodeBlock
translateLet isRec binds body = do
  -- The local slots used by this let expression
  slots <- reserveFrameSlots (length binds)
  let indexedBinds = zip binds slots
  -- Compute the extended environment used for letrec
  -- (creates an indirection closure for each local bind)
  bindsEnv <- forM indexedBinds $ \(bind, slot) -> do
    label <- freshName (fromName (fst bind))
    saveCodeBlock label [ EnterI (ArgM slot) ]
    return (fst bind, LabelM label)
  -- The environment used for the RHS of a let bind
  let rhsEnv | isRec     = bindsEnv
             | otherwise = []
  -- Translate the let right-hand sides
  bindsCode <- withExtendedEnv rhsEnv $ do
    traverse (uncurry translateLetBind) indexedBinds
  -- Translate the body with the extended bind environment
  exprCode <- withExtendedEnv bindsEnv $ do
     translateExpr body
  return (mconcat bindsCode <> exprCode)

translateLetBind :: MonadArepa m => (Name, CoreExpr) -> Int -> Translate m CodeBlock
translateLetBind bind slot = do
  whenVerbose $ dump "Translating let bind" (prettyPrint bind)
  rhsMode <- translateArgMode (snd bind)
  return [ MoveI slot rhsMode ]

----------------------------------------
-- Addressing modes

translateArgMode :: MonadArepa m => CoreExpr -> Translate m ArgMode
translateArgMode expr = do
  whenVerbose $ dump "Translating address mode of expression" (prettyPrint expr)
  case expr of
    VarE name -> do
      lookupArgMode name
    LitE lit -> do
      value <- translateLit lit
      return (ValueM value)
    _ -> do
      label <- freshName "arg"
      code <- translateExpr expr
      saveCodeBlock label code
      return (LabelM label)

translateValueMode :: MonadArepa m => Lit -> Translate m ValueMode
translateValueMode lit = do
  whenVerbose $ dump "Translating value mode of literal" (prettyPrint lit)
  value <- translateLit lit
  return (InlineM value)

----------------------------------------
-- Literals

translateLit :: MonadArepa m => Lit -> Translate m Value
translateLit lit = do
  whenVerbose $ dump "Translating literal" (prettyPrint lit)
  case lit of
    IntL    n -> return (IntV n)
    DoubleL n -> return (DoubleV n)
    StringL s -> return (StringV s)

----------------------------------------
-- Utilities

-- A pattern to identify calls to primitive operations

pattern CallE :: Name -> [CoreExpr] -> CoreExpr
pattern CallE name args <- (isCallE -> Just (name, args))

isCallE :: CoreExpr -> Maybe (Name, [CoreExpr])
isCallE expr =
  case collectArgs expr of
    (VarE name, args) | name `isPrimOp` primitives -> Just (name, args)
    _ -> Nothing
