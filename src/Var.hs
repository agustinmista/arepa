module Var 
  ( Var
  , mkVar
  , varName 
  ) where

import Data.String

import Data.Text (Text)
import Data.Text qualified as Text

import Prettyprinter

----------------------------------------
-- Variables
----------------------------------------

-- A simple variable opaque type for now. 
-- We will likely need to expand it in the future.

newtype Var = Var Text 
  deriving (Show, Read, Eq, Ord) 

-- Constructors/destructors

mkVar :: Text -> Var
mkVar = Var

varName :: Var -> Text 
varName (Var t) = t

-- This instance let us write variables directly as strings:
-- >>> "foo" :: Var) 
-- "Var \"foo\""
instance IsString Var where
  fromString s = Var (Text.pack s)

instance Pretty Var where
  pretty v = pretty (varName v) 
