module Language.Arepa.Compiler.Rename
  ( renameModule
  ) where

import Control.Monad.Extra
import Control.Monad.State

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
  renamedDecls <- runRenamer name $ do
    subst <- topLevelSubst (declName <$> decls)
    inLocalScopeWith subst $ do
      mapM renameDecl decls
  return (m { modDecls = renamedDecls })

----------------------------------------
-- Renaming internal state

data RenamerState = RenamerState {
  rs_module_name :: Name,
  rs_subst :: Map Name Name
}

initialRenamerState :: Name -> RenamerState
initialRenamerState name = RenamerState {
  rs_module_name = name,
  rs_subst = Map.empty
}

----------------------------------------
-- Renaming monad

type Renamer m a = StateT RenamerState m a

runRenamer :: MonadArepa m => Name -> Renamer m a -> m a
runRenamer moduleName ma = evalStateT ma (initialRenamerState moduleName)

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
      zEncode name
    Just name' -> do
      whenVerbose $ debug ("Found renamed name for " <> prettyPrint name <> ": " <> prettyPrint name')
      return name'

-- Rename a name uniquely, adding the substitution to the current scope
renameUnique :: MonadArepa m => Name -> Renamer m Name
renameUnique name = do
  st <- get
  name' <- liftIO (mkUniqueName (rs_module_name st) name)
  whenVerbose $ debug ("Renaming " <> prettyPrint name <> " as " <> prettyPrint name')
  put st { rs_subst = Map.insert name name' (rs_subst st) }
  return name'

-- z-encode a name, adding the substitution to the current scope
zEncode :: MonadArepa m => Name -> Renamer m Name
zEncode name = do
  st <- get
  let name' = zEncodeName name
  liftIO (registerUsedName name')
  whenVerbose $ debug ("Z-encoding name " <> prettyPrint name <> " as " <> prettyPrint name')
  put st { rs_subst = Map.insert name name' (rs_subst st) }
  return name'

-- Run the renamer in a local environment applying a name substitution
inLocalScopeWith :: MonadArepa m => [(Name, Name)] -> Renamer m a -> Renamer m a
inLocalScopeWith subst ma = do
  whenVerbose $ dump "Running in local scope with substitution" subst
  st <- get
  put (st { rs_subst = foldr (uncurry Map.insert) (rs_subst st) subst })
  a <- ma
  modify' $ \st' -> st' { rs_subst = rs_subst st }
  whenVerbose $ dump "Restored the previous scope to" (Map.toList (rs_subst st))
  return a

inLocalScope :: MonadArepa m => Renamer m a -> Renamer m a
inLocalScope = inLocalScopeWith []

-- Create the top-level substitution of names.
-- NOTE: this makes sure to avoid renaming the entry point, as well as taking
-- primitives into account. Other globals are only z-encoded to keep linking
-- against external code predictable.
topLevelSubst :: MonadArepa m => [Name] -> Renamer m [(Name, Name)]
topLevelSubst declNames = do
  -- Primitives: no renaming, but we must make sure to register them
  let primNames = Map.keys primitives
  let primSubsts = [ (pn, pn) | pn <- primNames ]
  liftIO $ mapM_ registerUsedName primNames
  -- Top-level declarations: we z-rename them, except for the entry point
  entry <- lookupCompilerOption optEntryPoint
  declSubsts <- forM declNames $ \dn -> do
    if dn == mkName entry
    then return (dn, dn)
    else zEncode dn >>= \dn' -> return (dn, dn')
  return (primSubsts <> declSubsts)

----------------------------------------
-- Renaming user code
----------------------------------------

-- Declarations

renameDecl :: MonadArepa m => CoreDecl -> Renamer m CoreDecl
renameDecl decl = do
  case decl of
    ValD name body -> do
      renameVal name body
    FunD name args body -> do
      renameFun name args body

renameVal :: MonadArepa m => Name -> CoreExpr -> Renamer m CoreDecl
renameVal name body = do
  inLocalScope $ do
    whenVerbose $ dump "Renaming value declaration" (name, body)
    name' <- getName name
    body' <- inLocalScope $ do
      renameExpr body
    return (ValD name' body')

renameFun :: MonadArepa m => Name -> [Name] -> CoreExpr -> Renamer m CoreDecl
renameFun name args body = do
  inLocalScope $ do
    whenVerbose $ dump "Renaming function declaration" (name, args, body)
    name' <- getName name
    args' <- mapM renameUnique args
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
    IfE cond th el -> do
      renameIf cond th el
    CaseE scrut alts -> do
      renameCase scrut alts
    SeqE e1 e2 -> do
      renameSeq e1 e2
    _ -> do
      return expr

-- Primitive calls

renameCall :: MonadArepa m => Name -> [CoreExpr] -> Renamer m CoreExpr
renameCall name args = do
  whenVerbose $ dump "Renaming primitive call" (name, args)
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
  inLocalScope $ do
    args' <- mapM renameUnique args
    let subst = zip args args'
    body' <- inLocalScopeWith subst $ do
      renameExpr body
    return (LamE args' body')

-- Let expressions

renameLet :: MonadArepa m => Bool -> [(Name, CoreExpr)] -> CoreExpr -> Renamer m CoreExpr
renameLet isRec binds body = do
  whenVerbose $ dump "Renaming let expression" (isRec, binds, body)
  inLocalScope $ do
    let (letVars, letRhss) = unzip binds
    letVars' <- mapM renameUnique letVars
    let subst = zip letVars letVars'
    letRhss' <- forM letRhss $ \expr -> do
      inLocalScopeWith subst $ do
        renameExpr expr
    let binds' = zip letVars' letRhss'
    body' <- inLocalScopeWith subst $
      renameExpr body
    return (LetE isRec binds' body')

-- Conditional expressions

renameIf :: MonadArepa m => CoreExpr -> CoreExpr -> CoreExpr -> Renamer m CoreExpr
renameIf cond th el = do
  whenVerbose $ dump "Renaming if expression" (cond, th, el)
  cond' <- renameExpr cond
  th' <- renameExpr th
  el' <- renameExpr el
  return (IfE cond' th' el')

-- Case expressions

renameCase :: MonadArepa m => CoreExpr -> [CoreAlt] -> Renamer m CoreExpr
renameCase scrut alts = do
  whenVerbose $ dump "Renaming case expression" (scrut, alts)
  scrut' <- renameExpr scrut
  alts'  <- mapM renameAlt alts
  return (CaseE scrut' alts')

renameAlt :: MonadArepa m => CoreAlt -> Renamer m CoreAlt
renameAlt alt = do
  whenVerbose $ dump "Renaming case alternative" alt
  case alt of
    ConA con vars body -> do
      vars' <- mapM renameUnique vars
      let subst = zip vars vars'
      body' <- inLocalScopeWith subst $ do
        renameExpr body
      return (ConA con vars' body')
    DefA var body -> do
      var' <- renameUnique var
      let subst = [(var, var')]
      body' <- inLocalScopeWith subst $ do
        renameExpr body
      return (DefA var' body')

-- Sequential expressions

renameSeq :: MonadArepa m => CoreExpr -> CoreExpr -> Renamer m CoreExpr
renameSeq e1 e2 = do
  whenVerbose $ dump "Renaming sequential expression" (e1, e2)
  e1' <- renameExpr e1
  e2' <- renameExpr e2
  return (SeqE e1' e2')