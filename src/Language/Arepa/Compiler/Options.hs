module Language.Arepa.Compiler.Options where

import Data.Name
import Language.TIM

----------------------------------------
-- Compiler options
----------------------------------------

data ArepaOpts = ArepaOpts {
  optInput :: Maybe FilePath,    -- Nothing means stdin
  optOutput :: Maybe FilePath,   -- Nothing means stdout
  optDump :: [DumpOpt],
  optVerbose :: Bool,
  optInterpret :: Bool,
  optEntryPoint :: Name,
  optInvokeArgs :: [Value],
  optOptimize :: Int,
  optDebug :: Bool
} deriving (Show, Read, Eq, Ord)

defaultOpts :: ArepaOpts
defaultOpts :: ArepaOpts = ArepaOpts {
  optInput = Nothing,
  optOutput = Nothing,
  optDump = [],
  optVerbose = False,
  optInterpret = False,
  optEntryPoint = "main",
  optInvokeArgs = [],
  optOptimize = 0,
  optDebug = False
}

-- Dump options
data DumpOpt = AST | PPR | TIM | LLVM
  deriving (Show, Read, Eq, Ord)
