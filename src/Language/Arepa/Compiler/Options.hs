module Language.Arepa.Compiler.Options where

import Data.Text.Lazy (Text)

----------------------------------------
-- Compiler options
----------------------------------------

data ArepaOpts = ArepaOpts {
  optInput :: Maybe FilePath,    -- Nothing means stdin
  optOutput :: Maybe FilePath,   -- Nothing means stdout
  optDump :: [DumpOpt],
  optVerbose :: Bool,
  optInterpret :: Bool,
  optEntryPoint :: Text
} deriving (Show, Read, Eq, Ord)

defaultOpts :: ArepaOpts
defaultOpts :: ArepaOpts = ArepaOpts {
  optInput = Nothing,
  optOutput = Nothing,
  optDump = [],
  optVerbose = False,
  optInterpret = False,
  optEntryPoint = "main"
}

-- Dump options
data DumpOpt = AST | PPR | TIM | LLVM
  deriving (Show, Read, Eq, Ord)
