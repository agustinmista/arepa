module Syntax where

import Data.Foldable
import Data.String
import Data.List

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Prettyprinter
import Prettyprinter.Render.Text

----------------------------------------
-- Modules
----------------------------------------

data Module a =
  Module {
    mod_name :: a,
    mod_decls :: [Decl a]
  } deriving (Show, Read, Eq, Ord, Functor)

type CoreModule = Module Var

instance Pretty CoreModule where
  pretty (Module name decls) =
    parens $ vsep $
      [ "module" <+> pretty name ] <>
      [ indent 2 (pretty decl) | decl <- decls ]

----------------------------------------
-- Top-level declarations
----------------------------------------

data Decl a =
    ValD a (Expr a)
  | FunD a [a] (Expr a)
  deriving (Show, Read, Eq, Ord, Functor)

type CoreDecl = Decl Var

instance Pretty CoreDecl where
  pretty (ValD name body)
    | isAtomicExpr body =
        parens $
          "val" <+> pretty name <+> pretty body
    | otherwise =
        parens $ vsep
          [ "val" <+> pretty name
          , indent 2 (pretty body)
          ]
  pretty (FunD name args body)
    | isAtomicExpr body =
        parens $
          "fun" <+> pretty name <+> parens (hsep (pretty <$> args)) <+> pretty body
    | otherwise =
      parens $ vsep
        [ "fun" <+> pretty name <+> parens (hsep (pretty <$> args))
        , indent 2 (pretty body)
        ]

----------------------------------------
-- Expressions
----------------------------------------

data Expr a =
    VarE a                           -- ^ Variables                
  | LitE Lit                         -- ^ Literals
  | ConE Con                         -- ^ Data constructors
  | AppE (Expr a) (Expr a)           -- ^ Function application
  | LamE a (Expr a)                  -- ^ Lambda expressions
  | LetE Bool [(a, Expr a)] (Expr a) -- ^ Let expressions
  | CaseE (Expr a) [Alt a]           -- ^ Case expressions
  deriving (Show, Read, Eq, Ord, Functor)

type CoreExpr = Expr Var

instance Pretty CoreExpr where
  pretty (VarE var) =
    pretty var
  pretty (LitE lit) =
    pretty lit
  pretty (ConE con) =
    pretty con
  pretty expr@AppE {} =
    let (fun, args) = collectArgs expr in
    parens $
      pretty fun <+> hsep (pretty <$> args)
  pretty (LamE var expr) =
    parens $
      "lambda" <+> pretty var <+> pretty expr
  pretty (LetE isRec binds expr) =
    parens $ vsep
      [ (if isRec then "letrec" else "let") <+>
          parens (align $ vsep $ [ parens (pretty v <+> pretty e) | (v, e) <- binds ])
      , indent 2 (pretty expr)
      ]
  pretty (CaseE expr alts) =
    parens $ vsep
      [ "case" <+> pretty expr
      , indent 2 $ parens $ align $ vsep $ pretty <$> alts
      ]

----------------------------------------
-- Expression construction

-- Create a variable expression
mkVarE :: a -> Expr a
mkVarE = VarE

-- Create an integer literal expression
mkIntLitE :: Int -> Expr a
mkIntLitE n = LitE (IntL n)

-- Create a double literal expression
mkDoubleLitE :: Double -> Expr a
mkDoubleLitE n = LitE (DoubleL n)

-- Create a char literal expression
mkCharLitE :: Char -> Expr a
mkCharLitE c = LitE (CharL c)

-- Create a string literal expression
mkStringLitE :: Text -> Expr a
mkStringLitE s = LitE (StringL s)

-- Create a constructor expression, applying a data constructor to a list of
-- argument expressions
mkConAppE :: Con -> [Expr a] -> Expr a
mkConAppE con = mkAppsE (ConE con)

-- Create a function application, applying an (function) expression to a list of
-- argument expressions 
mkAppsE :: Expr a -> [Expr a] -> Expr a
mkAppsE = foldl' AppE

-- Create a lambda expression by abstracting a list of variables from a body
-- expression 
mkLamsE :: [a] -> Expr a -> Expr a
mkLamsE vars body = foldr LamE body vars

----------------------------------------
-- Expression utilities

-- Is this expression atomic?
isAtomicExpr :: Expr a -> Bool
isAtomicExpr VarE {} = True
isAtomicExpr LitE {} = True
isAtomicExpr ConE {} = True
isAtomicExpr _       = False

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
    LitA Lit (Expr a)     -- ^ Literal alternative: 'case e of { 1 -> ... }' 
  | ConA Con [a] (Expr a) -- ^ Constructor alternative: 'case e of { C x y -> ... }'
  | DefA (Expr a)         -- ^ Default alternative: 'case e of { _ -> ... }'
  deriving (Show, Read, Eq, Ord, Functor)

type CoreAlt = Alt Var

instance Pretty CoreAlt where
  pretty (LitA lit expr) =
    parens $
      pretty lit <+> pretty expr
  pretty (ConA con vars expr) =
    parens $
      parens (pretty con <> cat (intersperse space (pretty <$> vars))) <+>
      pretty expr
  pretty (DefA expr) =
    parens $
      "_" <+> pretty expr

----------------------------------------
-- Literals
----------------------------------------

data Lit =
    IntL Int
  | DoubleL Double
  | CharL Char
  | StringL Text
  deriving (Show, Eq, Read, Ord)

instance Pretty Lit where
  pretty (IntL n)    = pretty n
  pretty (DoubleL n) = pretty n
  pretty (CharL c)   = pretty (show c)
  pretty (StringL s) = pretty (show s)

----------------------------------------
-- Data constructors
----------------------------------------

data Con =
    BoxedC   Int Int    -- ^ Boxed constructor:  { tag, arity }   
  | UnboxedC Int [Int]  -- ^ Unboxed constructor { tag, [size] }
  deriving (Show, Read, Eq, Ord)

instance Pretty Con where
  pretty (BoxedC tag arity) =
    braces (pretty tag <> comma <> pretty arity)
  pretty (UnboxedC tag sizes) =
    braces (pretty tag <> comma <> brackets (cat (intersperse comma (pretty <$> sizes))))

----------------------------------------
-- Variables
----------------------------------------

-- A simple variable opaque type for now. 
-- We will likely need to expand it in the future.

newtype Var = Var Text
  deriving (Show, Read, Eq, Ord)

-- Constructors/destructors

mkVar :: Text -> Var
mkVar = Var

fromVar :: IsString a => Var -> a
fromVar (Var t) = fromString (Text.unpack t)

-- This instance let us write variables directly as strings
instance IsString Var where
  fromString s = mkVar (Text.pack s)

instance Pretty Var where
  pretty (Var v) = pretty v

----------------------------------------
-- Pretty printing utilities
----------------------------------------

ppr :: Pretty a => a -> Text
ppr a = renderLazy (layoutPretty defaultLayoutOptions (pretty a))
