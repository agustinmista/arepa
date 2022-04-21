module Language.Arepa.Compiler.Translate
  ( translateModule
  ) where

import Control.Monad.State

import Data.Map (Map)
import Data.Map qualified as Map

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad

import Language.TIM.Syntax

----------------------------------------
-- Arepa to TIM translation
----------------------------------------

translateModule :: MonadArepa m => CoreModule -> m CodeStore
translateModule m = do
  let name = mod_name m
  let globals = declName <$> mod_decls m
  runTranslate name globals $ do
    mapM_ translateDecl (mod_decls m)

----------------------------------------
-- Internal translation state

data TranslateState = TranslateState {
  ts_store :: CodeStore,              -- ^ The code store we build under the hood
  ts_env   :: Map Name AddressMode,   -- ^ The current stuff in scope
  ts_fresh :: Int                     -- ^ A source of fresh names
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
lookupAddressMode :: MonadArepa m => Name -> Translate m AddressMode
lookupAddressMode name = do
  env <- gets ts_env
  case Map.lookup name env of
    Nothing -> throwInternalError ("lookupAddressMode: variable " <> fromName name <> " is not in the environment")
    Just mode -> return mode

-- Run the translation inside a local environment
withExtendedEnv :: MonadArepa m => [(Name, AddressMode)] -> Translate m a -> Translate m a
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

----------------------------------------
-- Translation operations

-- Declarations

translateDecl :: MonadArepa m => CoreDecl -> Translate m ()
translateDecl decl = do
  let args = declArgs decl
  let takeArgs = [ TakeI (length args) ]
  let extEnv = zip args (ArgM <$> [0..])
  bodyInstrs <- withExtendedEnv extEnv $ do
    translateExpr (declBody decl)
  saveCodeBlock (declName decl) (takeArgs <> bodyInstrs)

-- Expressions

translateExpr :: MonadArepa m => CoreExpr -> Translate m CodeBlock
translateExpr expr = do
  case expr of
    VarE name -> do
      mode <- translateExprMode (VarE name)
      return [EnterI mode]
    LitE lit -> do
      mode <- translateExprMode (LitE lit)
      return [EnterI mode]
    ConE _con -> do
      notImplemented "translateExpr/ConE"
    AppE e1 e2 -> do
      e1code <- translateExpr e1
      mode <- translateExprMode e2
      return ([PushI mode] <> e1code)
    LamE _var _expr -> do
      notImplemented "translateExpr/LamE"
    LetE _isRec _binds _expr -> do
      notImplemented "translateExpr/LetE"
    CaseE _scrut _alts -> do
      notImplemented "translateExpr/CaseE"

-- Addressing modes

translateExprMode :: MonadArepa m => CoreExpr -> Translate m AddressMode
translateExprMode expr = do
  case expr of
    VarE name -> do
      lookupAddressMode name
    LitE lit -> do
      return (LitM lit)
    _ -> do
      label <- freshName "fun"
      code <- translateExpr expr
      saveCodeBlock label code
      return (LabelM label)
