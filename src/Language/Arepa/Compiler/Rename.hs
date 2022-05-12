{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Language.Arepa.Compiler.Rename
  ( renameModule
  ) where

import Control.Monad.Extra
import Control.Monad.State

import Data.List

import Data.Map (Map)
import Data.Map qualified as Map

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad

import Language.TIM.Prim

----------------------------------------
-- Lambda lifting
----------------------------------------

renameModule :: MonadArepa m => CoreModule -> m CoreModule
renameModule m = do
  let name = modName m
  let decls = modDecls m
  let globals = fmap declName decls <> Map.keys primitives
  whenVerbose $ debug ("Renaming module " <> prettyPrint name)
  when (hasDuplicates globals) $ do
    throwRenamerError ("duplicated top-level declarations: " <> prettyPrint (findDuplicates globals))
  renamedDecls <- runRenamer globals (mapM renameDecl decls)
  return (m { modDecls = renamedDecls })

----------------------------------------
-- Lambda lifting internal state

data RenamerState = RenamerState {
  rs_scope :: Map Name Name
}

initialRenamerState :: [Name] ->  RenamerState
initialRenamerState globals = RenamerState {
  rs_scope = Map.fromList [ (name, name) | name <- globals ]
}

----------------------------------------
-- Lambda lifting monad

type Renamer m a = StateT RenamerState m a

runRenamer :: MonadArepa m => [Name] -> Renamer m a -> m a
runRenamer globals ma = evalStateT ma (initialRenamerState globals)

----------------------------------------
-- Monad operations

-- Lookup a variable name. Might have been renamed in an outer scope.
-- NOTE: we assume out of scope variables are going to be linked against some
-- external definition, unless --strict is enabled
getVarName :: MonadArepa m => Name -> Renamer m Name
getVarName name = do
  scope <- gets rs_scope
  case Map.lookup name scope of
    Nothing -> do
      whenM hasStrictEnabled $ do
        throwRenamerError $ "variable " <> prettyPrint name <> " is out of scope"
      return name
    Just name' ->
      return name'

-- (Possibly) rename a variable
-- NOTE: returns the same name unless it shadows some other name in scope
renameVar :: MonadArepa m => Name -> Renamer m Name
renameVar name = do
  st <- get
  let rename ss = mkName (fromName name <> head ss)
  let go ss | rename ss `notElem` rs_scope st = rename ss
            | otherwise = go (tail ss)
  let name' = go ("" : [ "#" <> show n | n <- [ 0 :: Int .. ] ])
  whenVerbose $ debug ("Renaming variable " <> prettyPrint name <> " as " <> prettyPrint name')
  put st { rs_scope = Map.insert name name' (rs_scope st) }
  return name'

-- Run the renamer in a local environment applying a name substitution
withExtendedScope :: MonadArepa m => [(Name, Name)] -> Renamer m a -> Renamer m a
withExtendedScope subst ma = do
  whenVerbose $ debug ("Extending scope with " <> prettyPrint subst)
  st <- get
  put (st { rs_scope = foldr (uncurry Map.insert) (rs_scope st) subst })
  a <- ma
  modify' $ \st' -> st' { rs_scope = rs_scope st }
  return a

----------------------------------------
-- Lifting user code
----------------------------------------

-- Declarations

renameDecl :: MonadArepa m => CoreDecl -> Renamer m CoreDecl
renameDecl decl = do
  whenVerbose $ dump "Renaming declaration" decl
  case decl of
    ValD name expr -> withExtendedScope [] $ do
      expr' <- renameExpr expr
      return (ValD name expr')
    FunD name args expr -> withExtendedScope [] $ do
      when (hasDuplicates args) $ do
        throwRenamerError ("duplicated function arguments: " <> prettyPrint (findDuplicates args))
      args' <- mapM renameVar args
      let subst = zip args args'
      expr' <- withExtendedScope subst $ do
        renameExpr expr
      return (FunD name args' expr')

-- Expressions

renameExpr :: MonadArepa m => CoreExpr -> Renamer m CoreExpr
renameExpr expr = do
  whenVerbose $ dump "Renaming expression" expr
  case expr of
    VarE name -> do
      name' <- getVarName name
      return (VarE name')
    AppE e1 e2 -> do
      e1' <- renameExpr e1
      e2' <- renameExpr e2
      return (AppE e1' e2')
    LamE args body -> do
      renameLambda args body
    LetE isRec binds body -> do
      renameLet isRec binds body
    CaseE scrut alts -> do
      scrut' <- renameExpr scrut
      alts'  <- mapM renameAlt alts
      return (CaseE scrut' alts')
    _ -> do
      return expr

-- Lambda expressions

renameLambda :: MonadArepa m => [Name] -> CoreExpr -> Renamer m CoreExpr
renameLambda args body = do
  when (hasDuplicates args) $ do
    throwRenamerError ("duplicated lambda function arguments: " <> prettyPrint (findDuplicates args))
  args' <- mapM renameVar args
  let subst = zip args args'
  body' <- withExtendedScope subst $ do
    renameExpr body
  return (LamE args' body')

-- Let expressions

renameLet :: MonadArepa m => Bool -> [(Name, CoreExpr)] -> CoreExpr -> Renamer m CoreExpr
renameLet isRec binds body = do
  let (letVars, letRhss) = unzip binds
  when (hasDuplicates letVars) $ do
    throwRenamerError ("duplicated let bindings: " <> prettyPrint (findDuplicates letVars))
  letVars' <- mapM renameVar letVars
  let subst = zip letVars letVars'
  letRhss' <- forM letRhss $ \expr -> do
    withExtendedScope subst $ do
      renameExpr expr
  let binds' = zip letVars' letRhss'
  body' <- withExtendedScope subst $
    renameExpr body
  return (LetE isRec binds' body')

-- Alternatives

renameAlt :: MonadArepa m => CoreAlt -> Renamer m CoreAlt
renameAlt alt = do
  whenVerbose $ dump "Renaming alternative" alt
  case alt of
    ConA con vars body -> do
      vars' <- mapM renameVar vars
      let subst = zip vars vars'
      body' <- withExtendedScope subst $ do
        renameExpr body
      return (ConA con vars' body')
    LitA lit body -> do
      body' <- renameExpr body
      return (LitA lit body')
    DefA body -> do
      body' <- renameExpr body
      return (DefA body')

----------------------------------------
-- Utilities

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates = not . null . findDuplicates

findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = fmap head (filter ((>1) . length) (group (sort xs)))