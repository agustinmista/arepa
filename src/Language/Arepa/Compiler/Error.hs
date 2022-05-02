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
  | InterpreterError InterpreterError
  | InternalError InternalError
  deriving Show

instance Pretty ArepaError where
  pretty (ParserError      err) = vsep ["Parser error:",      pretty (errorBundlePretty err)]
  pretty (InterpreterError err) = vsep ["Interpreter error:", pretty err]
  pretty (InternalError    err) = vsep ["Internal error:",    pretty err]

-- Parse errors
type ParserError = ParseErrorBundle Text Void

-- Interpreter errors
type InterpreterError = TIMError

-- Internal errors
type InternalError = Text
