module Lit where

----------------------------------------
-- Literals
----------------------------------------

data Lit =
    IntL Int
  | DoubleL Double
  | CharL Char
  | StringL String
  deriving (Show, Eq, Read, Ord)