module Language.Arepa.Compiler.Translate
  ( translateModule
  , interpretCodeStore
  ) where

import Control.Monad.Extra
import Control.Monad.State

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Text.Lazy (Text)

import Prettyprinter

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
  let decls = declName <$> mod_decls m
  let prims = Map.keys primitives
  let globals = prims <> decls
  runTranslate name globals $ do
    mapM_ translateDecl (mod_decls m)

-- Interpret a TIM code store, invoking some function
interpretCodeStore :: MonadArepa m => CodeStore -> m [Value]
interpretCodeStore store = do
  entry <- lookupCompilerOption optEntryPoint
  (res, trace) <- liftIO $ do
    runTIM store $ invokeFunction entry
  whenM hasVerboseEnabled $ do
    debugMsg "interpreter intermediate states" (Just (prettyPrint trace))
  case res of
    Left err -> throwInterpreterError err
    Right vals -> return vals

----------------------------------------
-- Internal translation state

data TranslateState = TranslateState {
  ts_store :: CodeStore,          -- ^ The code store we build under the hood
  ts_env   :: Map Name ArgMode,   -- ^ The current stuff in scope
  ts_fresh :: Int                 -- ^ A source of fresh names
}

initialTranslateState :: Name -> [Name] -> TranslateState
initialTranslateState name globals = TranslateState {
  ts_store = emptyCodeStore name,
  ts_env = Map.fromList [ (global, LabelM global) | global <- globals ],
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

-- Lookup for the addressing mode of a variable in the environment
lookupArgMode :: MonadArepa m => Name -> Translate m ArgMode
lookupArgMode name = do
  env <- gets ts_env
  case Map.lookup name env of
    Nothing -> throwInternalError ("lookupArgMode: variable " <> fromName name <> " is not in the environment")
    Just mode -> return mode

-- Run the translation inside a local environment
withExtendedEnv :: MonadArepa m => [(Name, ArgMode)] -> Translate m a -> Translate m a
withExtendedEnv binds ma = do
  st <- get
  put (st { ts_env = foldr (uncurry Map.insert) (ts_env st) binds })
  a <- ma
  modify' $ \st' -> st' { ts_env = ts_env st }
  return a

-- Insert a code bind in the internal code store
saveCodeBlock :: MonadArepa m => Name -> CodeBlock -> Translate m ()
saveCodeBlock name code = modify $ \st ->
  st { ts_store = insertCodeStore name code (ts_store st) }

-- Return a fresh name with a given prefix
freshName :: MonadArepa m => Text -> Translate m Name
freshName prefix = state $ \st ->
  (mkNameWithNum prefix (ts_fresh st), st { ts_fresh = ts_fresh st + 1 })

-- Lookup a primitive operation by its name
lookupPrimOp :: MonadArepa m => Name -> Translate m Prim
lookupPrimOp name = do
  case Map.lookup name primitives of
    Nothing -> throwInternalError ("lookupPrimOp: primitive " <> fromName name <> " is missing")
    Just prim -> return prim

----------------------------------------
-- Translation operations

-- Declarations

translateDecl :: MonadArepa m => CoreDecl -> Translate m ()
translateDecl decl = do
  let args = declArgs decl
  let takeArgs = [ TakeArgI (length args) ]
  let extEnv = zip args (ArgM <$> [0..])
  bodyInstrs <- withExtendedEnv extEnv $ do
    translateExpr (declBody decl)
  saveCodeBlock (declName decl) (takeArgs <> bodyInstrs)

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
      mode <- translateExprArgMode (VarE name)
      return [EnterI mode]
    -- Literal values
    LitE lit -> do
      mode <- translateLitValueMode lit
      return [PushValueI mode, ReturnI]
    -- Data constructors
    ConE _con -> do
      notImplemented "translateExpr/ConE"
    -- Function application
    AppE e1 e2 -> do
      e1code <- translateExpr e1
      mode <- translateExprArgMode e2
      return ([PushArgI mode] <> e1code)
    -- Lambda functions should be gone by now
    LamE _var _expr -> do
      notImplemented "translateExpr/LamE"
    -- Let bindings
    LetE _isRec _binds _expr -> do
      notImplemented "translateExpr/LetE"
    -- Case expressions
    CaseE _scrut _alts -> do
      notImplemented "translateExpr/CaseE"

-- A pattern to identify calls to primitive operations

pattern CallE :: Name -> [CoreExpr] -> CoreExpr
pattern CallE name args <- (isCallE -> Just (name, args))

isCallE :: CoreExpr -> Maybe (Name, [CoreExpr])
isCallE expr =
  case collectArgs expr of
    (VarE name, args) | name `isPrimOp` primitives -> Just (name, args)
    _ -> Nothing

-- Primitive function calls

-- This uses continuation passing style to build the chain of functions that
-- prepare the value stack to call the primitive operation.

translateCall :: MonadArepa m => Name -> [CoreExpr] -> Translate m CodeBlock
translateCall name args = do
  prim <- lookupPrimOp name

  when (prim_arity prim /= length args) $ do
    throwInternalError ("translateCall: primitive " <> fromName name <> " must take exactly " <> pretty (prim_arity prim) <> " arguments")

  translateCallArgs args [CallI name, ReturnI]

translateCallArgs :: MonadArepa m => [CoreExpr] -> CodeBlock -> Translate m CodeBlock
translateCallArgs args cont = do
  case args of
    [] -> do
      return cont
    LitE lit : rest -> do
      mode <- translateLitValueMode lit
      translateCallArgs rest ([PushValueI mode] <> cont)
    expr : rest -> do
      label <- freshName "call_arg_cont"
      saveCodeBlock label cont
      code <- translateExpr expr
      translateCallArgs rest ([PushArgI (LabelM label)] <> code)

-- Addressing modes

translateExprArgMode :: MonadArepa m => CoreExpr -> Translate m ArgMode
translateExprArgMode expr = do
  case expr of
    VarE name -> do
      lookupArgMode name
    LitE lit -> do
      value <- translateLit lit
      return (ValueM value)
    _ -> do
      label <- freshName "fun_arg"
      code <- translateExpr expr
      saveCodeBlock label code
      return (LabelM label)

translateLitValueMode :: MonadArepa m => Lit -> Translate m ValueMode
translateLitValueMode lit = do
  value <- translateLit lit
  return (InlineM value)

-- Literals

translateLit :: MonadArepa m => Lit -> Translate m Value
translateLit lit =
  case lit of
    IntL n -> return (IntV n)
    DoubleL n -> return (DoubleV n)
    CharL c -> return (CharV c)
    StringL s -> return (StringV s)