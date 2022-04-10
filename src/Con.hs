module Con where

import Prettyprinter

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


instance Pretty a => Pretty (Con a) where
  pretty c = pretty (con_id c)