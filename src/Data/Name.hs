module Data.Name where

import Data.String

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Prettyprinter

----------------------------------------
-- Names
----------------------------------------

newtype Name = Name Text
  deriving (Show, Read, Eq, Ord)

-- Constructors/destructors

mkName :: String -> Name
mkName = Name . Text.pack

mkNameWithNum :: Text -> Int -> Name
mkNameWithNum prefix n = Name (prefix <> Text.pack (show n))

fromName :: IsString a => Name -> a
fromName (Name t) = fromString (Text.unpack t)

-- This instance let us write variables directly as strings
instance IsString Name where
  fromString = mkName

instance Pretty Name where
  pretty (Name v) = pretty v

