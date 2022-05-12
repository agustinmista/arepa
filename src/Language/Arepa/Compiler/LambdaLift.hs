module Language.Arepa.Compiler.LambdaLift
  ( lambdaLiftModule
  ) where

import Control.Monad.Extra
import Control.Monad.State

import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NonEmpty

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Map qualified as Map

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad

import Language.TIM.Prim

----------------------------------------
-- Lambda lifting
----------------------------------------

lambdaLiftModule :: MonadArepa m => CoreModule -> m CoreModule
lambdaLiftModule m = do
  let name = modName m
  let decls = modDecls m
  let globals = declName <$> decls
  whenVerbose $ debug ("Lambda lifting module " <> prettyPrint name)
  (oldDecls, liftedDecls) <- runLifter globals (mapM liftDecl decls)
  return (m { modDecls = oldDecls <> liftedDecls })

----------------------------------------
-- Lambda lifting internal state

data LifterState = LifterState {
  ls_globals :: Set Name,
  ls_scopes  :: NonEmpty (Set Name),
  ls_new_decls :: [CoreDecl]
}

initialLifterState :: [Name] ->  LifterState
initialLifterState globals = LifterState {
  ls_globals = Set.fromList (globals <> Map.keys primitives),
  ls_scopes  = [Set.empty],
  ls_new_decls = []
}

----------------------------------------
-- Lambda lifting monad

type Lifter m a = StateT LifterState m a

runLifter :: MonadArepa m => [Name] -> Lifter m a -> m (a, [CoreDecl])
runLifter globals ma = do
  (a, st) <- runStateT ma (initialLifterState globals)
  return (a, ls_new_decls st)

----------------------------------------
-- Monad operations

mkUniqueName :: MonadArepa m => Name -> Lifter m Name
mkUniqueName template = do
  globals <- gets ls_globals
  newDecls <- gets ls_new_decls
  let avoid = globals <> Set.fromList (declName <$> newDecls)
  let go xs | template `notElem` avoid = mkNameWithNum (fromName template) (head xs)
            | otherwise = go (tail xs)
  return (go [1 ..])

-- Lift an expression into a top-level declaration
makeTopLevel :: MonadArepa m => Name -> [Name] -> CoreExpr -> Lifter m Name
makeTopLevel name vars expr = do
  st <- get
  uname <- mkUniqueName name
  let decl = FunD uname vars expr
  put st { ls_new_decls = decl : ls_new_decls st }
  whenVerbose $ dump ("Lifted lambda to top-level declaration " <> prettyPrint uname) decl
  return uname

-- Check if a variable is free in the current scope
isFreeVar :: MonadArepa m => Name -> Lifter m Bool
isFreeVar name = do
  globals <- gets ls_globals
  localScope <- NonEmpty.head <$> gets ls_scopes
  return (name `notElem` globals <> localScope)

-- Run the lifter with some new variables bound in the current scope
withNewBoundedVars :: MonadArepa m => [Name] -> Lifter m a -> Lifter m a
withNewBoundedVars vars ma = do
  whenVerbose $ dump "Extending the current scope with" vars
  st <- get
  let localScope :| outerScope = ls_scopes st
  put (st { ls_scopes = foldr Set.insert localScope vars :| outerScope })
  a <- ma
  modify' $ \st' -> st' { ls_scopes = ls_scopes st }
  whenVerbose $ debug "Restored the previous scope"
  return a

-- Create a new scope to acummulate mark what's free and what's not
withNewLambdaScope :: MonadArepa m => [Name] -> Lifter m a -> Lifter m a
withNewLambdaScope vars ma = do
  whenVerbose $ debug "Creating new lambda scope"
  st <- get
  put (st { ls_scopes = Set.fromList vars <| ls_scopes st })
  a <- ma
  modify' $ \st' -> st' { ls_scopes = ls_scopes st }
  whenVerbose $ debug "Restored the previous scope"
  return a

----------------------------------------
-- Lifting user code
----------------------------------------

-- Declarations

liftDecl :: MonadArepa m => CoreDecl -> Lifter m CoreDecl
liftDecl decl = do
  whenVerbose $ dump "Lambda lifting declaration" decl
  case decl of
    ValD name expr -> do
      (_, expr') <- liftExpr expr
      return (ValD name expr')
    FunD name args expr -> do
      (_, expr') <- withNewBoundedVars args (liftExpr expr)
      return (FunD name args expr')

-- Expressions

liftExpr :: MonadArepa m => CoreExpr -> Lifter m (Set Name, CoreExpr)
liftExpr expr = do
  whenVerbose $ dump "Lambda lifting expression" expr
  case expr of
    VarE name -> do
      isFree <- isFreeVar name
      let fvs | isFree    = Set.singleton name
              | otherwise = Set.empty
      return (fvs, VarE name)
    AppE e1 e2 -> do
      (fvs1, e1') <- liftExpr e1
      (fvs2, e2') <- liftExpr e2
      return (fvs1 <> fvs2, AppE e1' e2')
    LamE vars body -> do
      liftLambda "lam" vars body
    LetE isRec binds body -> do
      liftLet isRec binds body
    CaseE scrut alts -> do
      (fvsScrut, scrut') <- liftExpr scrut
      (fvsAlts, alts') <- unzip <$> mapM liftAlt alts
      return (fvsScrut <> mconcat fvsAlts, CaseE scrut' alts')
    _ -> do
      return (Set.empty, expr)

-- Lambda expressions

liftLambda :: MonadArepa m => Name -> [Name] -> CoreExpr -> Lifter m (Set Name, CoreExpr)
liftLambda name vars body = do
  withNewLambdaScope vars $ do
    (fvs, body') <- liftExpr body
    let args = vars <> Set.toList fvs
    globalName <- makeTopLevel name args body'
    let appE = foldl AppE (VarE globalName) (VarE <$> Set.toList fvs)
    return (fvs, appE)

-- Let expressions

liftLet :: MonadArepa m => Bool -> [(Name, CoreExpr)] -> CoreExpr -> Lifter m (Set Name, CoreExpr)
liftLet isRec binds body = do
  let letVars = fst <$> binds
  (fvsBinds, binds') <- fmap unzip $ forM binds $ \(name, expr) -> do
    let bindVars | isRec     = letVars
                 | otherwise = [name]
    withNewBoundedVars bindVars $ do
      case expr of
        LamE vars lamBody -> do
          (fvs, expr') <- liftLambda name vars lamBody
          return (fvs, (name, expr'))
        _ -> do
          (fvs, expr') <- liftExpr expr
          return (fvs, (name, expr'))
  (fvsBody, body') <- withNewBoundedVars letVars (liftExpr body)
  return (mconcat fvsBinds <> fvsBody, LetE isRec binds' body')

-- Alternatives

liftAlt :: MonadArepa m => CoreAlt -> Lifter m (Set Name, CoreAlt)
liftAlt alt = do
  whenVerbose $ dump "Lambda lifting alternative" alt
  case alt of
    ConA con vars body -> do
      (fvs, body') <- withNewBoundedVars vars $ do
        liftExpr body
      return (fvs, ConA con vars body')
    _ -> do
      return (Set.empty, alt)
