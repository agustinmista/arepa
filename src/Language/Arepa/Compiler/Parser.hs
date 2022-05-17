module Language.Arepa.Compiler.Parser
  ( Parser
  , parseModule
  , parseDecl
  , parseExpr
  ) where

import Control.Monad.Extra

import Data.Maybe
import Data.Void
import Data.Functor

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad


----------------------------------------
-- Language parser
----------------------------------------

parseModule :: MonadArepa m => Text -> m CoreModule
parseModule text = do
  whenVerbose $ dump "Parsing module" text
  runParser (contents module') text

parseDecl :: MonadArepa m => Text -> m CoreDecl
parseDecl text = do
  whenVerbose $ dump "Parsing declaration" text
  runParser (contents decl) text

parseExpr :: MonadArepa m => Text -> m CoreExpr
parseExpr text = do
  whenVerbose $ dump "Parsing expression" text
  runParser (contents expr) text

----------------------------------------
-- The parser monad

-- A parsing transformer that runs on top of the compiler monad
type Parser m a = ParsecT Void Text m a

runParser :: MonadArepa m => Parser m a -> Text -> m a
runParser parser text = do
  path <- fromMaybe "<interactive>" <$> lookupCompilerOption optInput
  res <- runParserT parser path text
  case res of
    Left errs -> throwParserError errs
    Right a   -> return a

----------------------------------------
-- Syntax parsers

-- Modules

module' :: MonadArepa m => Parser m CoreModule
module' = label "module" $ do
  parens $ do
    keyword "module"
    nm <- name
    decls <- many decl
    return (Module nm decls)

-- Top-level declarations

decl :: MonadArepa m => Parser m CoreDecl
decl = label "decl" $ do
  parens $ do
    valD <|> funD

valD :: MonadArepa m => Parser m CoreDecl
valD = do
  keyword "val"
  nm <- name
  body <- expr
  return (ValD nm body)

funD :: MonadArepa m => Parser m CoreDecl
funD = do
  keyword "fun"
  nm <- name
  args <- parens $ some name
  body <- expr
  return (FunD nm args body)

-- Expressions

expr :: MonadArepa m => Parser m CoreExpr
expr = label "expression" $ do
  parenE <|> atomE

atomE :: MonadArepa m => Parser m CoreExpr
atomE = label "atomic expression" $ do
  try litE <|> varE <|> conE

parenE :: MonadArepa m => Parser m CoreExpr
parenE = label "s-expression" $ do
  parens $ do
    try lamE <|> try letE <|> try ifE <|> try caseE <|> appE

appE :: MonadArepa m => Parser m CoreExpr
appE = do
  fun <- expr
  args <- some expr
  return (foldl AppE fun args)

varE :: MonadArepa m => Parser m CoreExpr
varE = VarE <$> name

litE :: MonadArepa m => Parser m CoreExpr
litE = LitE <$> lit

conE :: MonadArepa m => Parser m CoreExpr
conE = ConE <$> con

lamE :: MonadArepa m => Parser m CoreExpr
lamE = do
  keyword "lambda"
  vs <- parens $ name `sepBy1` whitespace
  b <- expr
  return (LamE vs b)

letE :: MonadArepa m => Parser m CoreExpr
letE = do
  isRec <- try (keyword "letrec" $> True)
           <|>  keyword "let"    $> False
  binds <- parens $ some $ parens $ (,) <$> name <*> expr
  body <- expr
  return (LetE isRec binds body)

ifE :: MonadArepa m => Parser m CoreExpr
ifE = do
  keyword "if"
  cond <- expr
  th <- expr
  el <- expr
  return (IfE cond th el)

caseE :: MonadArepa m => Parser m CoreExpr
caseE = do
  keyword "case"
  scrut <- expr
  alts <- many alt
  return (CaseE scrut alts)

-- Alternatives

alt :: MonadArepa m => Parser m CoreAlt
alt = label "case alternative" $ do
  parens $ do
    (c, vars) <- parens $ (,) <$> con <*> many name
    e <- expr
    return (Alt c vars e)

-- Literals

lit :: MonadArepa m => Parser m Lit
lit = label "literal" $ do
  try doubleL <|> intL <|> stringL <|> boolL

intL :: MonadArepa m => Parser m Lit
intL = do
  n <- signed decimal
  return (IntL n)

doubleL :: MonadArepa m => Parser m Lit
doubleL = do
  n <- signed float
  return (DoubleL n)

stringL :: MonadArepa m => Parser m Lit
stringL = StringL <$> stringLiteral

boolL :: MonadArepa m => Parser m Lit
boolL = (keyword "true" $>  BoolL True)
    <|> (keyword "false" $> BoolL False)

-- Data constructors

con :: MonadArepa m => Parser m Con
con = label "data constructor" $ do
  braces $ do
    tag <- decimal
    comma
    arity <- decimal
    return (Con tag arity)

-- Variables

name :: MonadArepa m => Parser m Name
name = mkName <$> identifier

----------------------------------------
-- Lexer
----------------------------------------

-- How to parse whitespaces (including ;; comments)
whitespace :: MonadArepa m => Parser m ()
whitespace = Lexer.space space1 (Lexer.skipLineComment ";") empty

contents :: MonadArepa m => Parser m a -> Parser m a
contents p = whitespace *> p <* eof

----------------------------------------
-- Parsing identifiers (roughly the same rules as in Scheme)

identInitialChar :: MonadArepa m => Parser m Char
identInitialChar = letterChar <|> satisfy (`elem` (".%!$&*/:<=>?^~#_\\" :: [Char]))

identSubsequentChar :: MonadArepa m => Parser m Char
identSubsequentChar = identInitialChar <|> digitChar <|> identPeculiarChar

identPeculiarChar :: MonadArepa m => Parser m Char
identPeculiarChar = satisfy (`elem` ("+-" :: [Char]))

peculiarIdent :: MonadArepa m => Parser m String
peculiarIdent = pure <$> (char '+' <|> char '-')
             <* notFollowedBy identSubsequentChar

normalIdent :: MonadArepa m => Parser m String
normalIdent = liftM2 (:) identInitialChar (many identSubsequentChar)

identifier :: MonadArepa m => Parser m String
identifier = Lexer.lexeme whitespace $ do
  let check i | i `notElem` reserved = return i
              | otherwise = fail ("keyword " <> show i <> "cannot be used as an identifier")
  check =<< normalIdent <|> peculiarIdent

----------------------------------------
-- Parsing keywords

reserved :: [String]
reserved = ["module", "lambda", "let", "letrec", "if", "true", "false", "case"]

keyword :: MonadArepa m => Text -> Parser m ()
keyword kw = void $ Lexer.lexeme whitespace $ do
  string kw <* notFollowedBy identInitialChar

----------------------------------------
-- Low-level lexemes

symbol :: MonadArepa m => Text -> Parser m Text
symbol = Lexer.symbol whitespace

signed :: (MonadArepa m, Num a) => Parser m a -> Parser m a
signed = Lexer.signed (pure ())

float :: MonadArepa m => Parser m Double
float = Lexer.float <* whitespace

decimal :: MonadArepa m => Parser m Int
decimal = Lexer.decimal <* whitespace

stringLiteral :: MonadArepa m => Parser m Text
stringLiteral = Text.pack <$> (char '\"' *> manyTill Lexer.charLiteral (char '\"')) <* whitespace

parens :: MonadArepa m => Parser m a -> Parser m a
parens = between (symbol "(") (symbol ")")

braces :: MonadArepa m => Parser m a -> Parser m a
braces = between (symbol "{") (symbol "}")

comma :: MonadArepa m => Parser m ()
comma = void $ symbol ","
