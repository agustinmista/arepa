module Language.TIM.Prim.TH
  ( mkPrimitives
  ) where

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
--   // END PROTOTYPES
--
-- Their corresponding Haskell types are:
--   __prim_add_int__ :: Int -> Int -> IO Int
--   __prim_print_int__ :: Int -> IO ()
--   __prim_pi__ :: IO Double
--
-- And, `mkPrimtives "primitives" "prim.h"` will generate:
--
--    mkPrimitives "primitives" "rts/include/prim.h"
--  ======>
--    primitives :: PrimMap
--    primitives
--      = Data.Map.Internal.fromList
--          [(Language.Arepa.Syntax.mkName "__prim_add_int__",
--            ((PrimOp 2)
--               ([Language.TIM.Syntax.IntT, Language.TIM.Syntax.IntT],
--                Language.TIM.Syntax.IntT))
--              (\ [Language.TIM.Syntax.IntV x1, Language.TIM.Syntax.IntV x2]
--                 -> (fmap Language.TIM.Syntax.IntV) ((__prim_add_int__ x1) x2))),
--           (Language.Arepa.Syntax.mkName "__prim_print_int__",
--            ((PrimOp 1)
--               ([Language.TIM.Syntax.IntT], Language.TIM.Syntax.VoidT))
--              (\ [Language.TIM.Syntax.IntV x1]
--                 -> (fmap Language.TIM.Syntax.VoidV) (__prim_print_int__ x1))),
--           (Language.Arepa.Syntax.mkName "__prim_pi__",
--            ((PrimOp 0) ([], Language.TIM.Syntax.DoubleT))
--              (\ [] -> (fmap Language.TIM.Syntax.DoubleV) __prim_pi__))]
--    foreign import ccall unsafe "__prim_add_int__" __prim_add_int__
--      :: Int -> Int -> IO Int
--    foreign import ccall unsafe "__prim_print_int__" __prim_print_int__
--      :: Int -> IO ()
--    foreign import ccall unsafe "__prim_pi__" __prim_pi__ :: IO Double

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
    let protoTy = mkHsType args ret
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
  let ty = TH.TupE [ Just (TH.ListE [ TH.ConE (typeToTypeConName a) | a <- args  ])
                   , Just (TH.ConE (typeToTypeConName ret)) ]
  runner <- mkPrimRunner name ret args
  return (TH.ConE 'Prim `TH.AppE` len `TH.AppE` ty `TH.AppE` runner)

mkPrimRunner :: Name -> Type -> [Type] -> Q TH.Exp
mkPrimRunner name ret args = do
  let argsVars = [ (a, TH.mkName ("x" <> show n)) | (a, n) <- zip args ([1..] :: [Int]) ]
  let funPat = [ TH.ConP (typeToValueConName a) [TH.VarP v] | (a, v) <- argsVars ]
  let cFun = TH.VarE (TH.mkName (fromName name))
  let cFunApp = foldl TH.AppE cFun [ TH.VarE v | (_, v) <- argsVars ]
  let fun = TH.VarE 'fmap `TH.AppE` TH.ConE (typeToValueConName ret) `TH.AppE` cFunApp
  return (TH.LamE [TH.ListP funPat] fun)

-- TH Utilities

-- Given a prototype type, generate its corresponding Haskell type
mkHsType :: [Type] -> Type -> TH.Type
mkHsType args ret =
  foldr
    (TH.AppT . TH.AppT TH.ArrowT)
    (TH.AppT (TH.ConT (TH.mkName "IO")) (typeToHsType ret))
    (typeToHsType <$> args)

-- Name mappings between TIM and Haskell

typeToHsType :: Type -> TH.Type
typeToHsType IntT    = TH.ConT ''Int
typeToHsType DoubleT = TH.ConT ''Double
typeToHsType StringT = TH.ConT ''String
typeToHsType VoidT   = TH.TupleT 0

typeToValueConName :: Type -> TH.Name
typeToValueConName IntT    = 'IntV
typeToValueConName DoubleT = 'DoubleV
typeToValueConName StringT = 'StringV
typeToValueConName VoidT   = 'VoidV

typeToTypeConName :: Type -> TH.Name
typeToTypeConName IntT    = 'IntT
typeToTypeConName DoubleT = 'DoubleT
typeToTypeConName StringT = 'StringT
typeToTypeConName VoidT   = 'VoidT

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
