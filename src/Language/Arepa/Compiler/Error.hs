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
  pretty (ParserError      err) = vsep ["Parser error:",      viaShow (errorBundlePretty err)]
  pretty (InterpreterError err) = vsep ["Interpreter error:", viaShow err]
  pretty (InternalError    err) = vsep ["Internal error:",    viaShow err]

-- Parse errors
type ParserError = ParseErrorBundle Text Void

-- Internal errors
type InternalError = Doc ()

-- Interpreter errors
type InterpreterError = TIMError