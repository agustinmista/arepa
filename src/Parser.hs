module Parser
  ( Parser
  , parseModule
  , parseDecl
  , parseExpr
  , parseTest
  ) where


import Control.Monad.Identity

import Data.Functor
import Data.Text (Text)
import Data.Text qualified as Text

import Data.Void
import Data.Maybe
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

import Compiler
import Syntax

----------------------------------------
-- IR parser
----------------------------------------

-- The monadic parser stack
type Parser = ParsecT Void Text Identity

parse :: Parser a -> Text -> Compiler a
parse p t = do
  st <- getCompilerState
  let file = fromMaybe "<interactive>" (cs_curr_file st)
  case runParser (whitespace *> p <* eof) file t of
    Right a -> return a
    Left errs -> throwCompilerError (PsError errs)

parseModule :: Text -> Compiler CoreModule
parseModule = parse module'

parseDecl :: Text -> Compiler CoreDecl
parseDecl = parse decl

parseExpr :: Text -> Compiler CoreExpr
parseExpr = parse expr

----------------------------------------
-- Syntax parsers
----------------------------------------

-- Modules

module' :: Parser CoreModule
module' = label "module" $ do
  parens $ do
    keyword "module"
    name <- var
    decls <- many decl
    return (Module name decls)

-- Top-level declarations 

decl :: Parser CoreDecl
decl = label "decl" $ do
  parens $ do
    choice [valD, funD]

valD :: Parser CoreDecl
valD = do
  keyword "val"
  name <- var
  body <- expr
  return (ValD name body)

funD :: Parser CoreDecl
funD = do
  keyword "fun"
  name <- var
  args <- parens $ many var
  body <- expr
  return (FunD name args body)

-- Expressions

expr :: Parser CoreExpr
expr = label "expression" $ do
  choice [parenE, atomE]

atomE :: Parser CoreExpr
atomE = try litE <|> varE <|> conE

parenE :: Parser CoreExpr
parenE = parens $ lamE <|> letE <|> caseE <|> appE

appE :: Parser CoreExpr
appE = do
  fun <- expr
  args <- some expr
  return (foldl AppE fun args)

varE :: Parser CoreExpr
varE = VarE <$> var

litE :: Parser CoreExpr
litE = LitE <$> lit

conE :: Parser CoreExpr
conE = ConE <$> con

lamE :: Parser CoreExpr
lamE = do
  keyword "lambda"
  v <- var
  b <- expr
  return (LamE v b)

letE :: Parser CoreExpr
letE = do
  isRec <- choice [ try (symbol "letrec" $> True), symbol "let" $> False ] 
  binds <- parens $ many $ parens $ (,) <$> var <*> expr
  body <- expr 
  return (LetE isRec binds body)

caseE :: Parser CoreExpr
caseE = do
  keyword "case"
  scrut <- expr  
  alts <- parens $ many alt
  return (CaseE scrut alts)

-- Alternatives

alt :: Parser CoreAlt
alt = label "case alternative" $ do
  parens $ choice [try litA, conA, defA]

litA :: Parser CoreAlt
litA = do
  l <- lit
  e <- expr
  return (LitA l e)

conA :: Parser CoreAlt
conA = do
  (c, vars) <- parens $ (,) <$> con <*> many var
  e <- expr
  return (ConA c vars e)  

defA :: Parser CoreAlt
defA = do
  symbol "_"
  e <- expr
  return (DefA e)

-- Literals

lit :: Parser Lit
lit = label "literal" $ do
  choice [try doubleL, intL, charL, stringL]

intL :: Parser Lit
intL = IntL <$> signed decimal

doubleL :: Parser Lit
doubleL = DoubleL <$> signed float

charL :: Parser Lit
charL = CharL <$> charLiteral

stringL :: Parser Lit
stringL = StringL <$> stringLiteral

-- Data constructors

con :: Parser Con
con = label "data constructor" $ do
  choice [try unboxedC, boxedC]

boxedC :: Parser Con
boxedC = do
  braces $ do
    tag <- decimal <* whitespace
    comma
    arity <- decimal <* whitespace
    return (BoxedC tag arity)

unboxedC :: Parser Con
unboxedC = do
  braces $ do
    tag <- decimal
    comma
    sizes <- brackets (decimal `sepBy` comma)
    return (UnboxedC tag sizes)

-- Variables

var :: Parser Var
var = mkVar <$> identifier

----------------------------------------
-- Lexer
----------------------------------------

-- How to parse whitespaces (including ;; comments)
whitespace :: Parser ()
whitespace = Lexer.space space1 (Lexer.skipLineComment ";") empty

-- Parsing identifiers (roughly the same rules as in Scheme)
identInitial :: Parser Char
identInitial = letterChar <|> satisfy (`elem` ("!$&*/:<=>?^_~" :: [Char]))

identSubsequent :: Parser Char
identSubsequent = identInitial <|> digitChar <|> identPeculiar

identPeculiar :: Parser Char
identPeculiar = satisfy (`elem` ("+-." :: [Char]))

identifier :: Parser Text
identifier = Lexer.lexeme whitespace $ do
  let normalIdent = do
        x <- identInitial
        xs <- many identSubsequent
        return (Text.pack (x:xs))
  let peculiarIdent = 
        choice [string "+", string "-"] <* 
        notFollowedBy identSubsequent
  normalIdent <|> peculiarIdent

-- Parse a keyword not followed by an identifier char
keyword :: Text -> Parser ()
keyword kw = void $ Lexer.lexeme whitespace $ do
  string kw <* notFollowedBy identInitial

symbol :: Text -> Parser ()
symbol s = void (Lexer.symbol whitespace s)

signed :: Num a => Parser a -> Parser a
signed = Lexer.signed whitespace

float :: Parser Double
float = Lexer.float <* whitespace

decimal :: Parser Int
decimal = Lexer.decimal <* whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') Lexer.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill Lexer.charLiteral (char '\"')

comma :: Parser ()
comma = symbol ","
