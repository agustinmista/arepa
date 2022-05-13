module Language.Arepa.Compiler.Error where

import Data.Void
import Data.Text.Lazy (Text)
import Text.Megaparsec
import Prettyprinter

import Language.TIM

----------------------------------------
-- Compiler errors
----------------------------------------

data ArepaError =
    ParserError ParserError
  | RenamerError RenamerError
  | InterpreterError InterpreterError
  | InternalError InternalError
  deriving Show

instance Pretty ArepaError where
  pretty (ParserError      err)         = vsep ["Parser error:",      pretty (errorBundlePretty err)]
  pretty (RenamerError     (path, err)) = vsep ["Renamer error:",     pretty path <> ":", pretty err]
  pretty (InterpreterError (path, err)) = vsep ["Interpreter error:", pretty path <> ":", pretty err]
  pretty (InternalError    (path, err)) = vsep ["Internal error:",    pretty path <> ":", pretty err]

-- Parse errors
type ParserError = ParseErrorBundle Text Void

-- Renamer errors
type RenamerError = (FilePath, Text)

-- Interpreter errors
type InterpreterError = (FilePath, TIMError)

-- Internal errors
type InternalError = (FilePath, Text)
