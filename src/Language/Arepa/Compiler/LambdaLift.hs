module Language.Arepa.Compiler.LambdaLift
  ( lambdaLiftModule
  ) where

import Control.Monad.Extra
import Control.Monad.State

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
  let globals = fmap declName decls <> Map.keys primitives
  whenVerbose $ debug ("Lambda lifting module " <> prettyPrint name)
  (oldDecls, liftedDecls) <- runLifter globals (mapM liftDecl decls)
  return (m { modDecls = oldDecls <> liftedDecls })

----------------------------------------
-- Lambda lifting internal state

data LifterState = LifterState {
  ls_globals :: Set Name,
  ls_new_decls :: [CoreDecl]
}

initialLifterState :: [Name] ->  LifterState
initialLifterState globals = LifterState {
  ls_globals = Set.fromList globals,
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

-- Lift an expression into a top-level declaration
makeTopLevel :: MonadArepa m => Name -> [Name] -> CoreExpr -> Lifter m Name
makeTopLevel prefix vars expr = do
  st <- get
  name' <- liftIO (mkUniqueName prefix)
  let decl = FunD name' vars expr
  put st { ls_new_decls = decl : ls_new_decls st }
  whenVerbose $ dump ("Lifted lambda to top-level declaration " <> prettyPrint name') decl
  return name'

isGlobalVar :: MonadArepa m => Name -> Lifter m Bool
isGlobalVar name = (name `elem`) <$> gets ls_globals

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
      (_, expr') <- liftExpr expr
      return (FunD name args expr')

-- Expressions

liftExpr :: MonadArepa m => CoreExpr -> Lifter m (Set Name, CoreExpr)
liftExpr expr = do
  whenVerbose $ dump "Lambda lifting expression" expr
  case expr of
    -- Variables
    VarE name -> do
      liftVar name
    -- Function applications
    AppE fun op -> do
      liftApp fun op
    -- Lambda expressions
    LamE vars body -> do
      liftLambda vars body
    -- Let expressions
    LetE isRec binds body -> do
      liftLet isRec binds body
    -- Conditional expressions
    CondE alts -> do
      liftCond alts
    -- Case expressions
    CaseE scrut alts -> do
      liftCase scrut alts
    _ -> do
      return (Set.empty, expr)

-- Variables

liftVar :: MonadArepa m => Name -> Lifter m (Set Name, CoreExpr)
liftVar name = do
  isGlobal <- isGlobalVar name
  let fvsVar | isGlobal  = Set.empty
             | otherwise = Set.singleton name
  return (fvsVar, VarE name)

-- Function applications

liftApp :: MonadArepa m => CoreExpr -> CoreExpr -> Lifter m (Set Name, CoreExpr)
liftApp fun op = do
  (fvsFun, fun') <- liftExpr fun
  (fvsOp, op')   <- liftExpr op
  return (fvsFun <> fvsOp, AppE fun' op')

-- Lambda expressions

liftLambda :: MonadArepa m => [Name] -> CoreExpr -> Lifter m (Set Name, CoreExpr)
liftLambda vars body = do
  (fvsBody, body') <- liftExpr body
  let fvsLam = fvsBody `closedOver` vars
  let args = Set.toList fvsLam <> vars
  globalName <- makeTopLevel "lam" args body'
  let appE = foldl AppE (VarE globalName) (VarE <$> Set.toList fvsLam)
  return (fvsLam, appE)

-- Let expressions

liftLet :: MonadArepa m => Bool -> [(Name, CoreExpr)] -> CoreExpr -> Lifter m (Set Name, CoreExpr)
liftLet isRec binds body = do
  let letVars = fst <$> binds
  (fvsBinds, binds') <- fmap unzip $ forM binds $ \(name, expr) -> do
    let bindVars | isRec     = letVars
                 | otherwise = [name]
    (fvsExpr, expr') <- liftExpr expr
    return (fvsExpr `closedOver` bindVars, (name, expr'))
  (fvsBody, body') <- liftExpr body
  let fvsLet = mconcat fvsBinds <> (fvsBody `closedOver` letVars)
  return (fvsLet, LetE isRec binds' body')

-- Conditional expressions

liftCond :: MonadArepa m => [(CoreExpr, CoreExpr)] -> Lifter m (Set Name, CoreExpr)
liftCond alts = do
  (fvsAlts, alts') <- fmap unzip <$> forM alts $ \(cond, body) -> do
    (fvsCond, cond') <- liftExpr cond
    (fvsBody, body') <- liftExpr body
    return (fvsCond <> fvsBody, (cond', body'))
  return (mconcat fvsAlts, CondE alts')

-- Case expressions

liftCase :: MonadArepa m => CoreExpr -> [CoreAlt] -> Lifter m (Set Name, CoreExpr)
liftCase scrut alts = do
  (fvsScrut, scrut') <- liftExpr scrut
  (fvsAlts,  alts')  <- unzip <$> mapM liftAlt alts
  return (fvsScrut <> mconcat fvsAlts, CaseE scrut' alts')

liftAlt :: MonadArepa m => CoreAlt -> Lifter m (Set Name, CoreAlt)
liftAlt alt = do
  whenVerbose $ dump "Lambda lifting alternative" alt
  case alt of
    Alt con vars body -> do
      (fvsAlt, body') <- liftExpr body
      return (fvsAlt `closedOver` vars, Alt con vars body')

----------------------------------------
-- Utilities

closedOver :: Set Name -> [Name] -> Set Name
closedOver set vars = Set.difference set (Set.fromList vars)