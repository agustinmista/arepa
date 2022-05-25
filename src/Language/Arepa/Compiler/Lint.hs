module Language.Arepa.Compiler.Lint
  ( lintModule
  ) where

import Control.Monad.Extra
import Control.Monad.State

import Data.List

import Data.Map qualified as Map

import Language.Arepa.Syntax
import Language.Arepa.Prim
import Language.Arepa.Compiler.Monad

----------------------------------------
-- Linting stuff
----------------------------------------

lintModule :: MonadArepa m => CoreModule -> m CoreModule
lintModule m = do
  let name = modName m
  let decls = modDecls m
  whenVerbose $ debug ("Linting module " <> prettyPrint name)
  lintedDecls <- runLinter $ do
    mapM lintDecl decls
  return (m { modDecls = lintedDecls })

----------------------------------------
-- Linting internal state

data LinterState = LinterState -- No state needed for now :)

initialLinterState :: LinterState
initialLinterState = LinterState

----------------------------------------
-- Linting monad

type Linter m a = StateT LinterState m a

runLinter :: MonadArepa m => Linter m a -> m a
runLinter ma = evalStateT ma initialLinterState

----------------------------------------
-- Monad operations

-- Lookup a primitive operation by its name
lookupPrimOp :: MonadArepa m => Name -> Linter m Prim
lookupPrimOp name = do
  whenVerbose $ debug ("Looking up for primitive operation " <> prettyPrint name)
  case Map.lookup name primitives of
    Nothing -> do
      throwInternalError ("lookupPrimOp: primitive " <> prettyPrint name <> " is missing")
    Just prim -> do
      whenVerbose $ dump ("Found primitive operation " <> prettyPrint name) (prim_arity prim, prim_type prim)
      return prim

----------------------------------------
-- Linting user code
----------------------------------------

-- Declarations

lintDecl :: MonadArepa m => CoreDecl -> Linter m CoreDecl
lintDecl decl = do
  case decl of
    ValD name body -> do
      lintVal name body
    FunD name args body -> do
      lintFun name args body

lintVal :: MonadArepa m => Name -> CoreExpr -> Linter m CoreDecl
lintVal name body = do
  whenVerbose $ dump "Linting value declaration" (name, body)
  body' <- lintExpr body
  return (ValD name body')

lintFun :: MonadArepa m => Name -> [Name] -> CoreExpr -> Linter m CoreDecl
lintFun name args body = do
  whenVerbose $ dump "Linting function declaration" (name, args, body)
  -- Check that the function arguments are unique
  when (hasDuplicates args) $ do
    throwLinterError ("duplicated function argument names " <> prettyPrint (findDuplicates args))
  body' <- lintExpr body
  return (FunD name args body')

-- Expressions

lintExpr :: MonadArepa m => CoreExpr -> Linter m CoreExpr
lintExpr expr = do
  case expr of
    CallE name args -> do
      lintCall name args
    AppE fun op -> do
      lintApp fun op
    LamE args body -> do
      lintLambda args body
    LetE isRec binds body -> do
      lintLet isRec binds body
    IfE cond th el -> do
      lintIf cond th el
    CaseE scrut alts -> do
      lintCase scrut alts
    SeqE e1 e2 -> do
      lintSeq e1 e2
    _ -> do
      return expr

-- Primitive calls

lintCall :: MonadArepa m => Name -> [CoreExpr] -> Linter m CoreExpr
lintCall name args = do
  whenVerbose $ dump "Linting primitive call" (name, args)
  prim <- lookupPrimOp name
  let arity = prim_arity prim
  -- Check that the primitive call is fully saturated
  when (arity /= length args) $ do
    throwLinterError (prettyPrint name <> " must take exactly " <> prettyPrint arity <> " arguments")
  args' <- mapM lintExpr args
  return (CallE name args')

-- Function applications

lintApp :: MonadArepa m => CoreExpr -> CoreExpr -> Linter m CoreExpr
lintApp fun op = do
  whenVerbose $ dump "Linting function application" (fun, op)
  fun' <- lintExpr fun
  op' <- lintExpr op
  return (AppE fun' op')

-- Lambda expressions

lintLambda :: MonadArepa m => [Name] -> CoreExpr -> Linter m CoreExpr
lintLambda args body = do
  whenVerbose $ dump "Linting lambda expression" (args, body)
  -- Check that the lambda arguments are unique
  when (hasDuplicates args) $ do
    throwLinterError ("duplicated lambda expression argument names " <> prettyPrint (findDuplicates args))
  body' <- lintExpr body
  return (LamE args body')

-- Let expressions

lintLet :: MonadArepa m => Bool -> [(Name, CoreExpr)] -> CoreExpr -> Linter m CoreExpr
lintLet isRec binds body = do
  whenVerbose $ dump "Linting let expression" (isRec, binds, body)
  let (letVars, letRhss) = unzip binds
  when (hasDuplicates letVars) $ do
    throwLinterError ("duplicated let binding names " <> prettyPrint (findDuplicates letVars))
  letRhss' <- mapM lintExpr letRhss
  let binds' = zip letVars letRhss'
  body' <- lintExpr body
  return (LetE isRec binds' body')

-- Conditional expressions

lintIf :: MonadArepa m => CoreExpr -> CoreExpr -> CoreExpr -> Linter m CoreExpr
lintIf cond th el = do
  whenVerbose $ dump "Linting if expression" (cond, th, el)
  cond' <- lintExpr cond
  th' <- lintExpr th
  el' <- lintExpr el
  return (IfE cond' th' el')

-- Case expressions

lintCase :: MonadArepa m => CoreExpr -> [CoreAlt] -> Linter m CoreExpr
lintCase scrut alts = do
  whenVerbose $ dump "Linting case expression" (scrut, alts)
  -- Separate the constructor from the default alternatives
  let (consAlts, defAlts) = partitionAlts alts
  -- Check that constructor tags are unique
  let tags = [ con_tag con | (con, _, _) <- consAlts ]
  when (hasDuplicates tags) $ do
    throwLinterError ("duplicated occurrence of tags " <> prettyPrint (findDuplicates tags))
  -- Check that default alternatives occur at most once
  when (length defAlts > 1) $ do
    throwLinterError ("duplicated default alterntives " <> prettyPrint (fst <$> defAlts))
  -- Lint all the subexpressions
  scrut' <- lintExpr scrut
  cons' <- mapM lintConA consAlts
  defs' <- mapM lintDefA defAlts
  -- Put the default alternative last (if any), and sort the constructor ones by
  -- their tag in increasing order
  let alts' = sort cons' <> defs'
  return (CaseE scrut' alts')

lintConA :: MonadArepa m => (Con, [Name], CoreExpr) -> Linter m CoreAlt
lintConA (con, vars, body) = do
  whenVerbose $ dump "Linting constructor alternative" (con, vars, body)
  -- Check that the constructor argument variables are all unique
  when (hasDuplicates vars) $ do
    throwLinterError ("duplicated constructor pattern variables " <> prettyPrint (findDuplicates vars))
  -- Check that the constructor has enough argument variables
  let arity = con_arity con
  when (arity /= length vars) $ do
    throwLinterError ("pattern for " <> prettyPrint con <> " must take exactly " <> prettyPrint arity <> " arguments")
  body' <- lintExpr body
  return (ConA con vars body')

lintDefA :: MonadArepa m => (Name, CoreExpr) -> Linter m CoreAlt
lintDefA (var, body) = do
  whenVerbose $ dump "Linting default alternative" (var, body)
  body' <- lintExpr body
  return (DefA var body')

-- Sequential expressions

lintSeq :: MonadArepa m => CoreExpr -> CoreExpr -> Linter m CoreExpr
lintSeq e1 e2 = do
  whenVerbose $ dump "Linting sequential expression" (e1, e2)
  e1' <- lintExpr e1
  e2' <- lintExpr e2
  return (SeqE e1' e2')

----------------------------------------
-- Utilities

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates = not . null . findDuplicates

findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = fmap head (filter ((>1) . length) (group (sort xs)))
