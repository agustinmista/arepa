module Language.TIM.Types where

import Prettyprinter

----------------------------------------
-- TIM types
----------------------------------------

data Type =
    IntT
  | DoubleT
  | CharT
  | StringT
  | VoidT
  deriving (Show, Eq, Read, Ord)

instance Pretty Type where
  pretty IntT    = "Int"
  pretty DoubleT = "Double"
  pretty CharT   = "Char"
  pretty StringT = "String"
  pretty VoidT   = "Void"