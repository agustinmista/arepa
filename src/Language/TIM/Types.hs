module Language.TIM.Types where

import Prettyprinter

----------------------------------------
-- TIM types
----------------------------------------

data Type =
    IntT
  | DoubleT
  | StringT
  | BoolT
  | VoidT
  | TagT
  deriving (Show, Eq, Read, Ord)

instance Pretty Type where
  pretty IntT    = "Int"
  pretty DoubleT = "Double"
  pretty StringT = "String"
  pretty BoolT   = "Bool"
  pretty VoidT   = "Void"
  pretty TagT    = "Tag"