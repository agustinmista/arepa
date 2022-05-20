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
  | UnitT
  | TagT
  deriving (Show, Eq, Read, Ord)

instance Pretty Type where
  pretty IntT    = "Int"
  pretty DoubleT = "Double"
  pretty StringT = "String"
  pretty BoolT   = "Bool"
  pretty UnitT   = "Unit"
  pretty TagT    = "Tag"