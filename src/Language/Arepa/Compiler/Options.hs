module Language.Arepa.Compiler.Options where

----------------------------------------
-- Compiler options
----------------------------------------

data ArepaOpts = ArepaOpts {
  optInput :: Maybe FilePath,    -- Nothing means stdin
  optOutput :: Maybe FilePath,   -- Nothing means a.out
  optNoLinking :: Bool,
  optDump :: [DumpOpt],
  optVerbose :: Bool,
  optInterpret :: Bool,
  optInterpretStdout :: FilePath,
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
  optNoLinking = False,
  optDump = [],
  optVerbose = False,
  optInterpret = False,
  optInterpretStdout = "out.txt",
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
