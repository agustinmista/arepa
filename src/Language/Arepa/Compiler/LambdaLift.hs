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

mkUniqueTopLevelName :: MonadArepa m => Name -> Lifter m Name
mkUniqueTopLevelName template = do
  globals <- gets ls_globals
  newDecls <- gets ls_new_decls
  let avoid     = globals <> Set.fromList (declName <$> newDecls)
  let rename ss = mkName (fromName template <> head ss)
  let go ss | rename ss `notElem` avoid = rename ss
            | otherwise = go (tail ss)
  let uname = go [ "#" <> show n | n <- [0 :: Int ..] ]
  return uname

-- Lift an expression into a top-level declaration
makeTopLevel :: MonadArepa m => Name -> [Name] -> CoreExpr -> Lifter m Name
makeTopLevel name vars expr = do
  st <- get
  uname <- mkUniqueTopLevelName name
  let decl = FunD uname vars expr
  put st { ls_new_decls = decl : ls_new_decls st }
  whenVerbose $ dump ("Lifted lambda to top-level declaration " <> prettyPrint uname) decl
  return uname

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
    VarE name -> do
      isGlobal <- isGlobalVar name
      let fvsVar | isGlobal  = Set.empty
                 | otherwise = Set.singleton name
      return (fvsVar, VarE name)
    AppE e1 e2 -> do
      (fvsE1, e1') <- liftExpr e1
      (fvsE2, e2') <- liftExpr e2
      return (fvsE1 <> fvsE2, AppE e1' e2')
    LamE vars body -> do
      liftLambda vars body
    LetE isRec binds body -> do
      liftLet isRec binds body
    CaseE scrut alts -> do
      (fvsScrut, scrut') <- liftExpr scrut
      (fvsAlts,  alts') <- unzip <$> mapM liftAlt alts
      return (fvsScrut <> mconcat fvsAlts, CaseE scrut' alts')
    _ -> do
      return (Set.empty, expr)

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

-- Alternatives

liftAlt :: MonadArepa m => CoreAlt -> Lifter m (Set Name, CoreAlt)
liftAlt alt = do
  whenVerbose $ dump "Lambda lifting alternative" alt
  case alt of
    ConA con vars body -> do
      (fvsAlt, body') <- liftExpr body
      return (fvsAlt `closedOver` vars, ConA con vars body')
    LitA lit body -> do
      (fvsAlt, body') <- liftExpr body
      return (fvsAlt, LitA lit body')
    DefA body -> do
      (fvsAlt, body') <- liftExpr body
      return (fvsAlt, DefA body')

----------------------------------------
-- Utilities

closedOver :: Set Name -> [Name] -> Set Name
closedOver set vars = Set.difference set (Set.fromList vars)