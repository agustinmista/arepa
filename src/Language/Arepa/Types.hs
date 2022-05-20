module Language.Arepa.Types where

import Prettyprinter

----------------------------------------
-- Arepa types
----------------------------------------

data Type =
    IntT
  | DoubleT
  | StringT
  | BoolT
  | VoidT
  deriving (Show, Eq, Read, Ord)

instance Pretty Type where
  pretty IntT    = "Int"
  pretty DoubleT = "Double"
  pretty StringT = "String"
  pretty BoolT   = "Bool"
  pretty VoidT   = "Void"