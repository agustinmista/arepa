module Con where

import Var

----------------------------------------
-- Data constructors
----------------------------------------

data Con a = 
  Con {
    con_id   :: a,       -- ^ Constructor identifier
    con_type :: Int,     -- ^ Constructor type identifier
    con_tag  :: Int,     -- ^ Constructor tag identifier inside a type
    con_uniq :: Int      -- ^ Constructor unique identifier
  } deriving (Show, Read, Eq, Ord, Functor)