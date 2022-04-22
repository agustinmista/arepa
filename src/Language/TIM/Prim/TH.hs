module Language.TIM.Prim.TH where

import Control.Monad

import Data.Void
import Data.Functor

import Data.List

import Data.Text.Lazy (Text)
import Data.Text.Lazy    qualified as Text
import Data.Text.Lazy.IO qualified as Text

import Data.Map qualified as Map

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

import Language.Haskell.TH (Q)
import Language.Haskell.TH qualified as TH

import Language.TIM.Syntax
import Language.TIM.Prim.Types


----------------------------------------
-- Primitive operations miner
----------------------------------------

-- Constants

beginComment :: Text
beginComment = "// BEGIN PROTOTYPES"

endComment :: Text
endComment = "// END PROTOTYPES"

----------------------------------------
-- Primitive prototype declarations

data Prototype = Prototype Name Type [Type]
  deriving (Show, Read, Eq, Ord)

----------------------------------------
-- Prototype parser

parsePrimHeaderFile :: FilePath -> IO [Prototype]
parsePrimHeaderFile path = do
  text <- Text.readFile path
  case Text.splitOn beginComment text of
    [_, protoToEnd] -> do
      case Text.splitOn endComment protoToEnd of
        [protos, _] -> do
          parsePrototypes path protos
        _ -> do
          missingComment endComment
    _ -> do
      missingComment beginComment

missingComment :: Text -> a
missingComment comment =
  error ("parsePrimHeaderFile: could not find " <> Text.unpack comment)

parsePrototypes :: FilePath -> Text -> IO [Prototype]
parsePrototypes path text = do
  case runParser (contents (many prototype)) path text of
    Left err -> error (errorBundlePretty err)
    Right protos -> return protos

type Parser a = Parsec Void Text a

prototype :: Parser Prototype
prototype = do
  ret <- type'
  name <- identifier
  args <- parens (type' `sepBy` comma)
  semi
  return (Prototype name ret args)

type' :: Parser Type
type' = symbol "Int"    $> IntT
    <|> symbol "Double" $> DoubleT
    <|> symbol "Char"   $> CharT
    <|> symbol "String" $> StringT
    <|> symbol "Void"   $> VoidT

----------------------------------------
-- Prototype lexer

-- How to parse whitespaces (including ;; comments)
whitespace :: Parser ()
whitespace = Lexer.space space1 (Lexer.skipLineComment "//") empty

contents :: Parser a -> Parser a
contents p = whitespace *> p <* eof

symbol :: Text -> Parser Text
symbol = Lexer.symbol whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

comma :: Parser ()
comma = void $ symbol ","

semi :: Parser ()
semi = void $ symbol ";"

identifier :: Parser Name
identifier = do
  name <- many (alphaNumChar <|> char '_')
  return (mkName (Text.pack name))

----------------------------------------
-- TH stuff

mkPrimitives :: String -> FilePath -> Q [TH.Dec]
mkPrimitives var path = do
  protos <- TH.runIO $ parsePrimHeaderFile path
  imports <- mkForeignImports protos
  mapping <- mkPrimOpMapping var protos
  return (mapping <> imports)

mkForeignImports :: [Prototype] -> Q [TH.Dec]
mkForeignImports protos = do
  forM protos $ \(Prototype name ret args) -> do
    let cName = fromName name
    let hsName = TH.mkName (fromName name)
    let protoTy = mkHsType ret args
    return (TH.ForeignD (TH.ImportF TH.CCall TH.Unsafe cName hsName protoTy))

mkPrimOpMapping :: String -> [Prototype] -> Q [TH.Dec]
mkPrimOpMapping var protos = do
  let hsName = TH.mkName var
  let hsType = TH.ConT ''PrimMap
  assocs <- mapM mkAssoc protos
  let body = TH.NormalB (TH.AppE (TH.VarE 'Map.fromList) (TH.ListE assocs))
  return [
      TH.SigD hsName hsType,
      TH.ValD (TH.VarP hsName) body []
    ]

mkAssoc :: Prototype -> Q TH.Exp
mkAssoc (Prototype name ret args) = do
  primOp <- mkPrimOp name ret args
  return (TH.TupE [ Just (mkPrimOpName name), Just primOp ])

mkPrimOpName :: Name -> TH.Exp
mkPrimOpName name =
  TH.AppE (TH.VarE 'mkName) (TH.LitE (TH.StringL (fromName name)))

mkPrimOp :: Name -> Type -> [Type] -> Q TH.Exp
mkPrimOp name ret args = do
  let len = TH.LitE (TH.IntegerL (genericLength args))
  runner <- mkPrimRunner name ret args
  return (TH.ConE 'PrimOp `TH.AppE` len `TH.AppE` runner)

mkPrimRunner :: Name -> Type -> [Type] -> Q TH.Exp
mkPrimRunner name ret args = do
  let argsVars = [ (arg, TH.mkName ("x" <> show n)) | (arg, n) <- zip args ([1..] :: [Int]) ]
  let funPat = [ TH.ConP (timTypeToConName arg) [TH.VarP v] | (arg, v) <- argsVars ]
  let cFun = TH.VarE (TH.mkName (fromName name))
  let cFunApp = foldl TH.AppE cFun [ TH.VarE v | (_, v) <- argsVars ]
  let fun = TH.VarE 'fmap `TH.AppE` TH.ConE (timTypeToConName ret) `TH.AppE` cFunApp
  return (TH.LamE [TH.ListP funPat] fun)


-- Utilities

mkHsType :: Type -> [Type] -> TH.Type
mkHsType ret args =
  foldr
    (TH.AppT . TH.AppT TH.ArrowT)
    (TH.AppT (TH.ConT (TH.mkName "IO")) (timToHsType ret))
    (timToHsType <$> args)

-- >>> mkHsType IntT [DoubleT, CharT]
-- AppT (AppT ArrowT (ConT Double)) (AppT (AppT ArrowT (ConT Char)) (AppT (ConT IO) (ConT Int)))

timToHsType :: Type -> TH.Type
timToHsType IntT    = TH.ConT ''Int
timToHsType DoubleT = TH.ConT ''Double
timToHsType CharT   = TH.ConT ''Char
timToHsType StringT = TH.ConT ''String
timToHsType VoidT   = TH.TupleT 0

timTypeToConName :: Type -> TH.Name
timTypeToConName IntT    = 'IntV
timTypeToConName DoubleT = 'DoubleV
timTypeToConName CharT   = 'CharV
timTypeToConName StringT = 'StringV
timTypeToConName VoidT   = 'VoidV