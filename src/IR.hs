module IR where

import Data.Foldable

import Lit
import Con

----------------------------------------
-- Binds
----------------------------------------

data Bind a =
    LetB a (Expr a)       -- ^ Non-recursive let bindings
  | RecB [(a, Expr a)]    -- ^ Mutually recursive bindings  
  deriving (Show, Read, Eq, Ord, Functor)

----------------------------------------
-- Bind utilities

bindersOf :: Bind a -> [a]
bindersOf (LetB a _) = [a]
bindersOf (RecB pairs) = fmap fst pairs

----------------------------------------
-- Expressions
----------------------------------------

data Expr a =
    VarE a                  -- ^ Variables                
  | LitE Lit                -- ^ Literals
  | ConE (Con a)            -- ^ Data constructors
  | AppE (Expr a) (Expr a)  -- ^ Function application
  | LamE a (Expr a)         -- ^ Lambda expressions
  | LetE (Bind a) (Expr a)  -- ^ Let expressions
  | CaseE [Alt a]           -- ^ Case expressions
  deriving (Show, Read, Eq, Ord, Functor)

----------------------------------------
-- Expression construction

-- Create a variable expression
mkVar :: a -> Expr a
mkVar = VarE

-- Create an integer literal expression
mkIntLit :: Int -> Expr a
mkIntLit n = LitE (IntL n)

-- Create a double literal expression
mkDoubleLit :: Double -> Expr a
mkDoubleLit n = LitE (DoubleL n)

-- Create a char literal expression
mkCharLit :: Char -> Expr a
mkCharLit c = LitE (CharL c)

-- Create a string literal expression
mkStringLit :: String -> Expr a
mkStringLit s = LitE (StringL s)

-- Create a constructor expression, applying a data constructor to a list of
-- argument expressions
mkConApp :: Con a -> [Expr a] -> Expr a
mkConApp con = mkApps (ConE con)

-- Create a function application, applying an (function) expression to a list of
-- argument expressions 
mkApps :: Expr a -> [Expr a] -> Expr a
mkApps = foldl' AppE

-- Create a lambda expression by abstracting a list of variables from a body
-- expression 
mkLams :: [a] -> Expr a -> Expr a
mkLams vars body = foldr LamE body vars

-- Create a non-recursive let expression
mkLet :: a -> Expr a -> Expr a -> Expr a
mkLet ident def = LetE (LetB ident def)

-- Create a mutually-recursive letrec expression 
mkLetRec :: [(a, Expr a)] -> Expr a -> Expr a
mkLetRec defs = LetE (RecB defs)

----------------------------------------
-- Expression utilities

-- Collect all the leading lambdas from an expression
collectBinders :: Expr a -> ([a], Expr a)
collectBinders = go []
  where
    go bs (LamE b e) = go (b:bs) e
    go bs e          = (reverse bs, e) 

-- Collect all the arguments from a function application
collectArgs :: Expr a -> (Expr a, [Expr a])
collectArgs = go []
  where
    go as (AppE f a) = go (a:as) f
    go as e          = (e, as)

----------------------------------------
-- Alternatives
----------------------------------------

-- We only support literal and flat constructor alternatives, with the addition
-- of the wildcard pattern `_` for convenience. The language frontend is the one
-- in charge of desugaring nested patterns into flat ones.

data Alt a =
    LitA Lit (Expr a)         -- ^ Literal alternative: 'case e of { 1 -> ... }' 
  | ConA (Con a) [a] (Expr a) -- ^ Constructor alternative: 'case e of { C x y -> ... }'
  | DefA (Expr a)             -- ^ Default alternative: 'case e of { _ -> ... }'
  deriving (Show, Read, Eq, Ord, Functor)