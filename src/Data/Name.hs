module Data.Name where


import Control.Monad

import Data.IORef
import Data.String

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Data.Set (Set)
import Data.Set qualified as Set

import Test.RandomStrings
import Prettyprinter
import Text.Encoding.Z
import GHC.IO (unsafePerformIO)

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

----------------------------------------
-- Creating globally unique names

{-# NOINLINE usedNames #-}
usedNames :: IORef (Set Name)
usedNames = unsafePerformIO (newIORef Set.empty)

registerUsedName :: Name -> IO ()
registerUsedName name = modifyIORef usedNames (Set.insert name)

-- Make a globally unique name based on a z-encoded prefix
mkUniqueName :: Name -> IO Name
mkUniqueName prefix = do
  avoid <- readIORef usedNames
  suffix <- replicateM 4 (onlyAlphaNum randomASCII)
  let name = mkName (zEncodeString (fromName prefix) <> "_" <> suffix)
  if name `notElem` avoid
    then modifyIORef usedNames (Set.insert name) >> return name
    else mkUniqueName prefix