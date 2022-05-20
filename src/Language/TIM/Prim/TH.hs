module Language.TIM.Prim.TH
  ( mkPrimitives
  ) where

import Control.Monad

import Foreign.C.String

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
import Language.TIM.Types
import Language.TIM.Prim.Types

----------------------------------------
-- Primitive operations miner
----------------------------------------

----------------------------------------
-- Primitive prototype declarations

data Prototype = Prototype Name Type [Type]
  deriving (Show, Read, Eq, Ord)

----------------------------------------
-- Template Haskell

-- `mkPrimitives var path` works by:
-- 1. Parsing the primitive prototypes declared in `path`
-- 2. Importing them as native functions using Haskell's FFI
-- 3. Define a top-level value `var :: PriMap` that can be used by:
--    + The interpreter, to call the C primitives at runtime
--    + The code generator, to generate the corresponding LLVM function calls
--
-- EXAMPLE: suppose the primitives file "rts/include/prim.h" contains:
--
--   // BEGIN PROTOTYPES
--   Int __prim_add_int__(Int, Int);
--   Void __prim_print_int__(Int);
--   Double __prim_pi__();
--   Void __prim_print_string__(String);
--   // END PROTOTYPES
--
-- Their corresponding Haskell types are:
--   __prim_add_int__ :: Int -> Int -> IO Int
--   __prim_print_int__ :: Int -> IO ()
--   __prim_pi__ :: IO Double
--   __prim_print_stirng__ :: CString -> IO ()

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
    let protoTy = foldr (TH.AppT . TH.AppT TH.ArrowT)
                        (TH.AppT (TH.ConT ''IO) (typeToHsType ret))
                        (typeToHsType <$> args)
    return (TH.ForeignD (TH.ImportF TH.CCall TH.Unsafe cName hsName protoTy))

mkPrimOpMapping :: String -> [Prototype] -> Q [TH.Dec]
mkPrimOpMapping var protos = do
  let hsName = TH.mkName var
  let hsType = TH.ConT ''PrimMap
  assocs <- mapM mkAssoc protos
  let body = TH.NormalB (
               TH.VarE 'Map.fromList `TH.AppE`
               TH.ListE assocs
             )
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
  TH.VarE 'mkName `TH.AppE`
  TH.LitE (TH.StringL (fromName name))

mkPrimOp :: Name -> Type -> [Type] -> Q TH.Exp
mkPrimOp name ret args = do
  let len = TH.LitE (TH.IntegerL (genericLength args))
  let ty = TH.TupE [ Just (TH.ListE [ TH.ConE (typeToTypeConName a) | a <- args  ])
                   , Just (TH.ConE (typeToTypeConName ret)) ]
  runner <- mkPrimRunner name ret args
  return (
      TH.ConE 'Prim `TH.AppE`
      len `TH.AppE`
      ty `TH.AppE`
      runner
    )

mkPrimRunner :: Name -> Type -> [Type] -> Q TH.Exp
mkPrimRunner name ret args = do
  let vars = [ TH.mkName ("x" <> show n) | n <- take (length args) ([1..] :: [Int]) ]
  let prime v = TH.mkName (TH.nameBase v <> "'")
  let cFunPat = [ TH.VarP v | v <- vars ]
  let cFunVar = TH.VarE (TH.mkName (fromName name))
  let cFunApp = foldl TH.AppE cFunVar [ TH.VarE (prime v) | v <- vars ]
  let cFun = foldl (\expr (t, v) ->
          TH.VarE (marshallArgName t) `TH.AppE`
          TH.VarE v `TH.AppE`
          TH.LamE [TH.VarP (prime v)] expr
        ) cFunApp (zip args vars)
  let fun = TH.VarE (marshallResName ret) `TH.AppE` cFun
  return (TH.LamE [TH.ListP cFunPat] fun)

----------------------------------------
-- Marshalling values between C and Haskell

-- Type name mappings between TIM and Haskell

typeToHsType :: Type -> TH.Type
typeToHsType IntT    = TH.ConT ''Int
typeToHsType DoubleT = TH.ConT ''Double
typeToHsType StringT = TH.ConT ''CString
typeToHsType BoolT   = TH.ConT ''Int
typeToHsType VoidT   = TH.TupleT 0
typeToHsType ty      = error ("typeToHsType: impossible type " <> show ty)

typeToTypeConName :: Type -> TH.Name
typeToTypeConName IntT    = 'IntT
typeToTypeConName DoubleT = 'DoubleT
typeToTypeConName StringT = 'StringT
typeToTypeConName BoolT   = 'BoolT
typeToTypeConName VoidT   = 'VoidT
typeToTypeConName TagT    = 'TagT

-- Marshalling arguments

marshallArgName :: Type -> TH.Name
marshallArgName IntT    = 'marshallArgInt
marshallArgName DoubleT = 'marshallArgDouble
marshallArgName StringT = 'marshallArgString
marshallArgName BoolT   = 'marshallArgBool
marshallArgName VoidT   = 'marshallArgVoid
marshallArgName ty      = error ("marshallArgName: impossible type " <> show ty)

marshallArgInt :: Value -> (Int -> IO a) -> IO a
marshallArgInt (IntV n) = \f -> f n
marshallArgInt _ = badMarshalling

marshallArgDouble :: Value -> (Double -> IO a) -> IO a
marshallArgDouble (DoubleV n) = \f -> f n
marshallArgDouble _ = badMarshalling

marshallArgString :: Value -> (CString -> IO a) -> IO a
marshallArgString (StringV text) = withCString (Text.unpack text)
marshallArgString _ = badMarshalling

marshallArgBool :: Value -> (Int -> IO a) -> IO a
marshallArgBool (BoolV b) = \f -> f (if b then 1 else 0)
marshallArgBool _ = badMarshalling

marshallArgVoid :: Value -> (() -> IO a) -> IO a
marshallArgVoid (VoidV u) = \f -> f u
marshallArgVoid _ = badMarshalling

-- Marshalling results

marshallResName :: Type -> TH.Name
marshallResName IntT    = 'marshallResInt
marshallResName DoubleT = 'marshallResDouble
marshallResName StringT = 'marshallResString
marshallResName BoolT   = 'marshallResBool
marshallResName VoidT   = 'marshallResVoid
marshallResName ty      = error ("marshallResName: impossible type " <> show ty)

marshallResInt :: IO Int -> IO Value
marshallResInt = fmap IntV

marshallResDouble :: IO Double -> IO Value
marshallResDouble = fmap DoubleV

marshallResString :: IO CString -> IO Value
marshallResString m = m >>= (fmap (StringV . Text.pack) . peekCString)

marshallResBool :: IO Int -> IO Value
marshallResBool = fmap (BoolV . (/= 0))

marshallResVoid :: IO () -> IO Value
marshallResVoid = fmap VoidV

badMarshalling :: a
badMarshalling = error "bad marshalling during primitive operation"

----------------------------------------
-- Prototype parser

-- Headers used to delimit primitive prototypes

beginComment :: Text
beginComment = "// BEGIN PROTOTYPES"

endComment :: Text
endComment = "// END PROTOTYPES"

-- Parse a file with primitive prototypes enclosed by the special comments
-- defined above. Fails at compile time if parsing doesn't succeed.

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

-- The parser combinators defining a primitive prototype

type Parser a = Parsec Void Text a

prototype :: Parser Prototype
prototype = do
  ret <- type'
  name <- identifier
  args <- parens (arg `sepBy` comma)
  semi
  return (Prototype name ret args)

type' :: Parser Type
type' = symbol "Int"    $> IntT
    <|> symbol "Double" $> DoubleT
    <|> symbol "String" $> StringT
    <|> symbol "Bool"   $> BoolT
    <|> symbol "Void"   $> VoidT

arg :: Parser Type
arg = do
  ty <- type'
  optional identifier
  return ty

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
  return (mkName name)
