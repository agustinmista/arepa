module Language.Arepa.Compiler.Options where

----------------------------------------
-- Compiler options
----------------------------------------

data ArepaOpts = ArepaOpts {
  optInput :: Maybe FilePath,    -- Nothing means stdin
  optOutput :: Maybe FilePath,   -- Nothing means stdout
  optDump :: [DumpOpt],
  optVerbose :: Bool,
  optInterpret :: Bool,
  optEntryPoint :: Maybe String,
  optOptimize :: Int,
  optStrict :: Bool,
  optEmitMain :: Bool,
  optInclude :: [FilePath],
  optDebug :: Bool
} deriving (Show, Read, Eq, Ord)

defaultOpts :: ArepaOpts
defaultOpts :: ArepaOpts = ArepaOpts {
  optInput = Nothing,
  optOutput = Nothing,
  optDump = [],
  optVerbose = False,
  optInterpret = False,
  optEntryPoint = Nothing,
  optOptimize = 0,
  optStrict = False,
  optEmitMain = False,
  optInclude = [],
  optDebug = False
}

-- Dump options
data DumpOpt = AST | PPR | TIM | LLVM
  deriving (Show, Read, Eq, Ord)
