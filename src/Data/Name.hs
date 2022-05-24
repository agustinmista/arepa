module Data.Name where

import GHC.IO (unsafePerformIO)

import Control.Monad

import Data.IORef
import Data.String

import Data.Hashable

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Data.Set (Set)
import Data.Set qualified as Set

import Test.RandomStrings
import Prettyprinter
import Text.Encoding.Z

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

zEncodeName :: Name -> Name
zEncodeName (Name t) = Name (Text.pack (zEncodeString (Text.unpack t)))

-- This instance let us write variables directly as strings
instance IsString Name where
  fromString = mkName

instance Pretty Name where
  pretty (Name name) = pretty name

instance Hashable Name where
  hashWithSalt n (Name name) = hashWithSalt n name

----------------------------------------
-- Creating globally unique names

{-# NOINLINE usedNames #-}
usedNames :: IORef (Set Name)
usedNames = unsafePerformIO (newIORef Set.empty)

registerUsedName :: Name -> IO ()
registerUsedName name = modifyIORef usedNames (Set.insert name)

-- Make a globally unique name based on a z-encoded prefix
mkUniqueName :: Name -> Name -> IO Name
mkUniqueName modName prefix = do
  avoid <- readIORef usedNames
  suffix <- replicateM 4 (onlyAlphaNum randomASCII)
  let name = mkName (zEncodeString (fromName modName) <> "_" <> zEncodeString (fromName prefix) <> "_" <> suffix)
  if name `notElem` avoid
    then modifyIORef usedNames (Set.insert name) >> return name
    else mkUniqueName modName prefix