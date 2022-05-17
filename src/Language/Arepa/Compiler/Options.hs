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
  optInterpretStdin :: FilePath,
  optInterpretStdout :: FilePath,
  optEntryPoint :: String,
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
  optInterpretStdin  = "/dev/stdin",
  optInterpretStdout = "/dev/stdout",
  optEntryPoint = "main",
  optOptimize = 0,
  optStrict = False,
  optEmitMain = False,
  optInclude = [],
  optDebug = False
}

-- Dump options
data DumpOpt = AST | PPR | RENAME | LIFT | TIM | LLVM
  deriving (Show, Read, Eq, Ord)
