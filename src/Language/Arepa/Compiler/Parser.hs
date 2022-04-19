module Language.Arepa.Compiler.Parser
  ( Parser
  , parseModule
  , parseDecl
  , parseExpr
  )
  where

import Control.Monad.Extra

import Data.Maybe
import Data.Void
import Data.Functor

import Data.Text.Lazy qualified as Text

import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad


----------------------------------------
-- Language parser
----------------------------------------

-- A parsing transformer that runs on top of the compiler monad
type Parser m a = ParsecT Void Text m a

runParser :: MonadArepa m => Parser m a -> Text -> m a
runParser parser text = do
  path <- fromMaybe "<interactive>" <$> lookupCompilerOption optInput
  res <- runParserT parser path text
  case res of
    Left errs -> throwParserError errs
    Right a   -> return a

parseModule :: MonadArepa m => Text -> m CoreModule
parseModule = runParser (contents module')

parseDecl :: MonadArepa m => Text -> m CoreDecl
parseDecl = runParser (contents decl)

parseExpr :: MonadArepa m => Text -> m CoreExpr
parseExpr = runParser (contents expr)


----------------------------------------
-- Syntax parsers
----------------------------------------

-- Modules

module' :: MonadArepa m => Parser m CoreModule
module' = label "module" $ do
  parens $ do
    keyword "module"
    name <- var
    decls <- many decl
    return (Module name decls)

-- Top-level declarations

decl :: MonadArepa m => Parser m CoreDecl
decl = label "decl" $ do
  parens $ do
    choice [valD, funD]

valD :: MonadArepa m => Parser m CoreDecl
valD = do
  keyword "val"
  name <- var
  body <- expr
  return (ValD name body)

funD :: MonadArepa m => Parser m CoreDecl
funD = do
  keyword "fun"
  name <- var
  args <- parens $ many var
  body <- expr
  return (FunD name args body)

-- Expressions

expr :: MonadArepa m => Parser m CoreExpr
expr = label "expression" $ do
  choice [parenE, atomE]

atomE :: MonadArepa m => Parser m CoreExpr
atomE = label "atomic expression" $ do
  choice [try litE, varE, conE]

parenE :: MonadArepa m => Parser m CoreExpr
parenE = label "s-expression" $ do
  parens $ do
    choice [try lamE, try letE, try caseE, appE]

appE :: MonadArepa m => Parser m CoreExpr
appE = do
  fun <- expr
  args <- some expr
  return (foldl AppE fun args)

varE :: MonadArepa m => Parser m CoreExpr
varE = VarE <$> var

litE :: MonadArepa m => Parser m CoreExpr
litE = LitE <$> lit

conE :: MonadArepa m => Parser m CoreExpr
conE = ConE <$> con

lamE :: MonadArepa m => Parser m CoreExpr
lamE = do
  keyword "lambda"
  v <- var
  b <- expr
  return (LamE v b)

letE :: MonadArepa m => Parser m CoreExpr
letE = do
  isRec <- choice [try (symbol "letrec" $> True), symbol "let" $> False]
  binds <- parens $ many $ parens $ (,) <$> var <*> expr
  body <- expr
  return (LetE isRec binds body)

caseE :: MonadArepa m => Parser m CoreExpr
caseE = do
  keyword "case"
  scrut <- expr
  alts <- parens $ many alt
  return (CaseE scrut alts)

-- Alternatives

alt :: MonadArepa m => Parser m CoreAlt
alt = label "case alternative" $ do
  parens $ choice [try litA, conA, defA]

litA :: MonadArepa m => Parser m CoreAlt
litA = do
  l <- lit
  e <- expr
  return (LitA l e)

conA :: MonadArepa m => Parser m CoreAlt
conA = do
  (c, vars) <- parens $ (,) <$> con <*> many var
  e <- expr
  return (ConA c vars e)

defA :: MonadArepa m => Parser m CoreAlt
defA = do
  symbol "_"
  e <- expr
  return (DefA e)

-- Literals

lit :: MonadArepa m => Parser m Lit
lit = label "literal" $ do
  choice [try doubleL, intL, charL, stringL]

intL :: MonadArepa m => Parser m Lit
intL = IntL <$> signed decimal

doubleL :: MonadArepa m => Parser m Lit
doubleL = DoubleL <$> signed float

charL :: MonadArepa m => Parser m Lit
charL = CharL <$> charLiteral

stringL :: MonadArepa m => Parser m Lit
stringL = StringL <$> stringLiteral

-- Data constructors

con :: MonadArepa m => Parser m Con
con = label "data constructor" $ do
  choice [try unboxedC, boxedC]

boxedC :: MonadArepa m => Parser m Con
boxedC = do
  braces $ do
    tag <- decimal
    comma
    arity <- decimal
    return (BoxedC tag arity)

unboxedC :: MonadArepa m => Parser m Con
unboxedC = do
  braces $ do
    tag <- decimal
    comma
    sizes <- brackets (decimal `sepBy` comma)
    return (UnboxedC tag sizes)

-- Variables

var :: MonadArepa m => Parser m Var
var = mkVar <$> identifier

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

identInitial :: MonadArepa m => Parser m Char
identInitial = letterChar <|> satisfy (`elem` ("!$&*/:<=>?^_~" :: [Char]))

identSubsequent :: MonadArepa m => Parser m Char
identSubsequent = identInitial <|> digitChar <|> identPeculiar

identPeculiar :: MonadArepa m => Parser m Char
identPeculiar = satisfy (`elem` ("+-." :: [Char]))

identifier :: MonadArepa m => Parser m Text
identifier = Lexer.lexeme whitespace $ do
  let normalIdent = do
        x <- identInitial
        xs <- many identSubsequent
        return (Text.pack (x:xs))
  let peculiarIdent =
        choice [string "+", string "-"] <*
        notFollowedBy identSubsequent
  let check i | i `notElem` reserved = return i
              | otherwise  = fail ("keyword " <> show i <> "cannot be used as an identifier")
  check =<< normalIdent <|> peculiarIdent

----------------------------------------
-- Parsing keywords

reserved :: [Text]
reserved = ["module", "lambda", "let", "letrec", "case"]

keyword :: MonadArepa m => Text -> Parser m ()
keyword kw = void $ Lexer.lexeme whitespace $ do
  string kw <* notFollowedBy identInitial

----------------------------------------
-- Low-level lexemes

symbol :: MonadArepa m => Text -> Parser m ()
symbol s = void (Lexer.symbol whitespace s)

signed :: (MonadArepa m, Num a) => Parser m a -> Parser m a
signed = Lexer.signed whitespace

float :: MonadArepa m => Parser m Double
float = Lexer.float <* whitespace

decimal :: MonadArepa m => Parser m Int
decimal = Lexer.decimal <* whitespace

parens :: MonadArepa m => Parser m a -> Parser m a
parens = between (symbol "(") (symbol ")")

braces :: MonadArepa m => Parser m a -> Parser m a
braces = between (symbol "{") (symbol "}")

brackets :: MonadArepa m => Parser m a -> Parser m a
brackets = between (symbol "[") (symbol "]")

charLiteral :: MonadArepa m => Parser m Char
charLiteral = between (char '\'') (char '\'') Lexer.charLiteral

stringLiteral :: MonadArepa m => Parser m Text
stringLiteral = Text.pack <$> (char '\"' *> manyTill Lexer.charLiteral (char '\"'))

comma :: MonadArepa m => Parser m ()
comma = symbol ","
