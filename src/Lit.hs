module Lit where

import Prettyprinter

----------------------------------------
-- Literals
----------------------------------------

data Lit =
    IntL Int
  | DoubleL Double
  | CharL Char
  | StringL String
  deriving (Show, Eq, Read, Ord)


instance Pretty Lit where
  pretty (IntL n) = pretty n
  pretty (DoubleL n) = pretty n
  pretty (CharL c) = "'" <> pretty c <> "'"
  pretty (StringL s) = pretty (show s)