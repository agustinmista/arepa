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
import Language.Arepa.Prim
import Language.Arepa.Compiler.Monad

----------------------------------------
-- Renaming stuff
----------------------------------------

renameModule :: MonadArepa m => CoreModule -> m CoreModule
renameModule m = do
  let name = modName m
  let decls = modDecls m
  whenVerbose $ debug ("Renaming module " <> prettyPrint name)
  renamedDecls <- runRenamer $ do
    subst <- topLevelSubst (declName <$> decls)
    inLocalScopeWith subst $ do
      mapM renameDecl decls
  return (m { modDecls = renamedDecls })

----------------------------------------
-- Lambda lifting internal state

data RenamerState = RenamerState {
  rs_subst :: Map Name Name
}

initialRenamerState :: RenamerState
initialRenamerState = RenamerState {
  rs_subst = Map.empty
}

----------------------------------------
-- Lambda lifting monad

type Renamer m a = StateT RenamerState m a

runRenamer :: MonadArepa m => Renamer m a -> m a
runRenamer ma = evalStateT ma initialRenamerState

----------------------------------------
-- Monad operations

-- Lookup a name. Might have been renamed in an outer scope.
-- NOTE: we assume out of scope variables are going to be linked against some
-- external definition, unless --strict is enabled
getName :: MonadArepa m => Name -> Renamer m Name
getName name = do
  scope <- gets rs_subst
  case Map.lookup name scope of
    Nothing -> do
      whenM hasStrictEnabled $ do
        throwRenamerError $ "variable " <> prettyPrint name <> " is out of scope"
      return name
    Just name' ->
      return name'

-- Rename a name, adding the substitution to the current scope
renameName :: MonadArepa m => Name -> Renamer m Name
renameName name = do
  st <- get
  name' <- liftIO (mkUniqueName name)
  whenVerbose $ debug ("Renaming variable " <> prettyPrint name <> " as " <> prettyPrint name')
  put st { rs_subst = Map.insert name name' (rs_subst st) }
  return name'

-- Run the renamer in a local environment applying a name substitution
inLocalScopeWith :: MonadArepa m => [(Name, Name)] -> Renamer m a -> Renamer m a
inLocalScopeWith subst ma = do
  whenVerbose $ debug ("Extending scope with " <> prettyPrint subst)
  st <- get
  put (st { rs_subst = foldr (uncurry Map.insert) (rs_subst st) subst })
  a <- ma
  modify' $ \st' -> st' { rs_subst = rs_subst st }
  return a

inLocalScope :: MonadArepa m => Renamer m a -> Renamer m a
inLocalScope = inLocalScopeWith []

-- Check for invalid duplicates

checkDuplicateNames :: MonadArepa m => [Name] -> Renamer m ()
checkDuplicateNames names = do
  when (hasDuplicates names) $ do
    throwRenamerError ("duplicated occurrence of names " <> prettyPrint (findDuplicates names))

checkDuplicateTags :: MonadArepa m => [CoreAlt] -> Renamer m ()
checkDuplicateTags alts = do
  let tags = [ con_tag con | Alt con _ _ <- alts ]
  when (hasDuplicates tags) $ do
    throwRenamerError ("duplicated occurrence of tags " <> prettyPrint (findDuplicates tags))


-- Check the arity of an operation

checkPrimArity :: MonadArepa m => Name -> [CoreExpr] -> Renamer m ()
checkPrimArity name args = do
  prim <- lookupPrimOp name
  let arity = prim_arity prim
  when (arity /= length args) $ do
    throwRenamerError (prettyPrint name <> " must take exactly " <> prettyPrint arity <> " arguments")

checkConArity :: MonadArepa m => Con -> [Name] -> Renamer m ()
checkConArity con args = do
  let arity = con_arity con
  when (arity /= length args) $ do
    throwRenamerError ("pattern for " <> prettyPrint con <> " must take exactly " <> prettyPrint arity <> " arguments")

-- Create the top-level substitution of names.
-- NOTE: this makes sure to avoid renaming the entry point, as well as taking
-- primitives into account.
topLevelSubst :: MonadArepa m => [Name] -> Renamer m [(Name, Name)]
topLevelSubst declNames = do
  -- Primitives: no renaming, but we must make sure to register them
  let primNames = Map.keys primitives
  let primSubsts = [ (pn, pn) | pn <- primNames ]
  mapM_ (liftIO . registerUsedName) primNames
  -- Top-level declarations: we rename them, except for the entry point
  entry <- lookupCompilerOption optEntryPoint
  declSubsts <- forM declNames $ \dn -> do
    if dn == mkName entry
    then return (dn, dn)
    else renameName dn >>= \dn' -> return (dn, dn')
  return (primSubsts <> declSubsts)

-- Lookup a primitive operation by its name
lookupPrimOp :: MonadArepa m => Name -> Renamer m Prim
lookupPrimOp name = do
  whenVerbose $ debug ("Looking up for primitive operation " <> prettyPrint name)
  case Map.lookup name primitives of
    Nothing -> do
      throwInternalError ("lookupPrimOp: primitive " <> prettyPrint name <> " is missing")
    Just prim -> do
      whenVerbose $ dump ("Found primitive operation " <> prettyPrint name) (prim_arity prim, prim_type prim)
      return prim

----------------------------------------
-- Renaming user code
----------------------------------------

-- Declarations

renameDecl :: MonadArepa m => CoreDecl -> Renamer m CoreDecl
renameDecl decl = do
  inLocalScope $ do
    whenVerbose $ dump "Renaming declaration" decl
    let name = declName decl
    let args = declArgs decl
    let body = declBody decl
    checkDuplicateNames args
    name' <- getName name
    args' <- mapM renameName args
    let subst = zip args args'
    body' <- inLocalScopeWith subst $ do
      renameExpr body
    return (FunD name' args' body')

-- Expressions

renameExpr :: MonadArepa m => CoreExpr -> Renamer m CoreExpr
renameExpr expr = do
  case expr of
    CallE name args -> do
      renameCall name args
    VarE name -> do
      renameVar name
    AppE fun op -> do
      renameApp fun op
    LamE args body -> do
      renameLambda args body
    LetE isRec binds body -> do
      renameLet isRec binds body
    CaseE scrut alts -> do
      renameCase scrut alts
    _ -> do
      return expr

-- Primitive calls

renameCall :: MonadArepa m => Name -> [CoreExpr] -> Renamer m CoreExpr
renameCall name args = do
  whenVerbose $ dump "Renaming primitive call" (name, args)
  checkPrimArity name args
  args' <- mapM renameExpr args
  return (CallE name args')

-- Variables

renameVar :: MonadArepa m => Name -> Renamer m CoreExpr
renameVar name = do
  whenVerbose $ dump "Renaming variable" name
  name' <- getName name
  return (VarE name')

-- Function applications

renameApp :: MonadArepa m => CoreExpr -> CoreExpr -> Renamer m CoreExpr
renameApp fun op = do
  whenVerbose $ dump "Renaming function application" (fun, op)
  fun' <- renameExpr fun
  op' <- renameExpr op
  return (AppE fun' op')

-- Lambda expressions

renameLambda :: MonadArepa m => [Name] -> CoreExpr -> Renamer m CoreExpr
renameLambda args body = do
  whenVerbose $ dump "Renaming lambda expression" (args, body)
  checkDuplicateNames args
  args' <- mapM renameName args
  let subst = zip args args'
  body' <- inLocalScopeWith subst $ do
    renameExpr body
  return (LamE args' body')

-- Let expressions

renameLet :: MonadArepa m => Bool -> [(Name, CoreExpr)] -> CoreExpr -> Renamer m CoreExpr
renameLet isRec binds body = do
  whenVerbose $ dump "Renaming let expression" (isRec, binds, body)
  let (letVars, letRhss) = unzip binds
  checkDuplicateNames letVars
  letVars' <- mapM renameName letVars
  let subst = zip letVars letVars'
  letRhss' <- forM letRhss $ \expr -> do
    inLocalScopeWith subst $ do
      renameExpr expr
  let binds' = zip letVars' letRhss'
  body' <- inLocalScopeWith subst $
    renameExpr body
  return (LetE isRec binds' body')

-- Case expressions

renameCase :: MonadArepa m => CoreExpr -> [CoreAlt] -> Renamer m CoreExpr
renameCase scrut alts = do
  whenVerbose $ dump "Renaming case expression" (scrut, alts)
  checkDuplicateTags alts
  scrut' <- renameExpr scrut
  alts'  <- mapM renameAlt alts
  return (CaseE scrut' alts')

renameAlt :: MonadArepa m => CoreAlt -> Renamer m CoreAlt
renameAlt alt = do
  whenVerbose $ dump "Renaming case alternative" alt
  case alt of
    Alt con vars body -> do
      checkConArity con vars
      checkDuplicateNames vars
      vars' <- mapM renameName vars
      let subst = zip vars vars'
      body' <- inLocalScopeWith subst $ do
        renameExpr body
      return (Alt con vars' body')

----------------------------------------
-- Utilities

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates = not . null . findDuplicates

findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = fmap head (filter ((>1) . length) (group (sort xs)))
