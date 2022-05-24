module Language.Arepa.Syntax
  ( module Language.Arepa.Syntax
  , module Data.Name
  ) where

import Data.Foldable

import Data.Either
import Data.List

import Data.Text.Lazy (Text)

import Prettyprinter

import Data.Name

----------------------------------------
-- Modules
----------------------------------------

data Module a =
  Module {
    modName :: a,
    modDecls :: [Decl a]
  } deriving (Show, Read, Eq, Ord, Functor)

type CoreModule = Module Name

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

type CoreDecl = Decl Name

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

declName :: CoreDecl -> Name
declName (ValD nm _) = nm
declName (FunD nm _ _) = nm

declArgs :: CoreDecl -> [Name]
declArgs (ValD _ _) = []
declArgs (FunD _ as _) = as

declBody :: CoreDecl -> CoreExpr
declBody (ValD _ body) = body
declBody (FunD _ _ body) = body

----------------------------------------
-- Expressions
----------------------------------------

data Expr a =
    VarE a                           -- ^ Variables
  | LitE Lit                         -- ^ Literals
  | ConE Con                         -- ^ Data constructors
  | AppE (Expr a) (Expr a)           -- ^ Function application
  | LamE [a] (Expr a)                -- ^ Lambda expressions
  | LetE Bool [(a, Expr a)] (Expr a) -- ^ Let expressions
  | IfE (Expr a) (Expr a) (Expr a)   -- ^ If expressions
  | CaseE (Expr a) [Alt a]           -- ^ Case expressions
  | SeqE (Expr a) (Expr a)           -- ^ Strict const
  deriving (Show, Read, Eq, Ord, Functor)

type CoreExpr = Expr Name

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
  pretty (LamE vars expr) =
    parens $
      "lambda" <+> parens (hsep (pretty <$> vars)) <+> pretty expr
  pretty (LetE isRec binds expr) =
    parens $ vsep
      [ (if isRec then "letrec" else "let") <+>
          parens (align $ vsep $ [ parens (pretty v <+> pretty e) | (v, e) <- binds ])
      , indent 2 (pretty expr)
      ]
  pretty (IfE cond th el) =
    parens $ vsep
      [ "if" <+> pretty cond
      , indent 2 $ align $ vsep [ pretty th, pretty el ]
      ]
  pretty (CaseE expr alts) =
    parens $ vsep
      [ "case" <+> pretty expr
      , indent 2 $ align $ vsep $ pretty <$> alts
      ]
  pretty (SeqE e1 e2) =
    parens $
      "seq" <+> align (vsep [ pretty e1, pretty e2 ])

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

----------------------------------------
-- Expression utilities

-- Is this expression atomic?
isAtomicExpr :: Expr a -> Bool
isAtomicExpr VarE {} = True
isAtomicExpr LitE {} = True
isAtomicExpr ConE {} = True
isAtomicExpr _       = False

-- Collect all the arguments from a function application
collectArgs :: Expr a -> (Expr a, [Expr a])
collectArgs = go []
  where
    go as (AppE f a) = go (a:as) f
    go as e          = (e, as)

----------------------------------------
-- Alternatives
----------------------------------------

-- NOTE: a case expression should have at most one default alternative. If such
-- alternative exists, it should be the first one of the list after renaming (GHC
-- Core style).

data Alt a =
    ConA Con [a] (Expr a)   -- ^ Constructor alternative: {m,n} v0 ... vn -> body
  | DefA Name (Expr a)      -- ^ Default alternative:     x               -> body
  deriving (Show, Read, Eq, Ord, Functor)

type CoreAlt = Alt Name

instance Pretty CoreAlt where
  pretty (ConA con [] expr) =
    parens $
      parens (pretty con) <+>
      pretty expr
  pretty (ConA con vars expr) =
    parens $
      parens (pretty con <+> cat (intersperse space (pretty <$> vars))) <+>
      pretty expr
  pretty (DefA var expr) =
    parens $
      pretty var <+>
      pretty expr

eitherAlt :: Alt a -> Either (Con, [a], Expr a) (Name, Expr a)
eitherAlt (ConA c vars body) = Left  (c, vars, body)
eitherAlt (DefA   var  body) = Right (var, body)

partitionAlts :: [Alt a] -> ([(Con, [a], Expr a)], [(Name, Expr a)])
partitionAlts alts = partitionEithers (eitherAlt <$> alts)

----------------------------------------
-- Literals
----------------------------------------

data Lit =
    IntL Int
  | DoubleL Double
  | StringL Text
  | BoolL Bool
  | UnitL
  deriving (Show, Eq, Read, Ord)

instance Pretty Lit where
  pretty (IntL n)      = pretty n
  pretty (DoubleL n)   = pretty n
  pretty (StringL s)   = pretty (show s)
  pretty (BoolL True)  = "true"
  pretty (BoolL False) = "false"
  pretty UnitL         = "unit"

----------------------------------------
-- Data constructors
----------------------------------------

data Con = Con {
  con_name :: Maybe Name,
  con_tag :: Int,
  con_arity :: Int
} deriving (Show, Read, Eq, Ord)

instance Pretty Con where
  pretty (Con (Just name) _ arity) =
    braces (pretty name <> comma <> pretty arity)
  pretty (Con Nothing tag arity) =
    braces (pretty tag <> comma <> pretty arity)
