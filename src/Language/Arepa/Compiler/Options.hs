module Language.Arepa.Compiler.Options where

----------------------------------------
-- Compiler options
----------------------------------------

data ArepaOpts = ArepaOpts {
  optInput :: FilePath,
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
  optBackend :: BackendOpt,
  optDebug :: Bool,
  optInteractive :: Bool
} deriving (Show, Read, Eq, Ord)

defaultOpts :: ArepaOpts
defaultOpts :: ArepaOpts = ArepaOpts {
  optInput = mempty,
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
  optBackend = LLVM,
  optDebug = False,
  optInteractive = False
}

-- Compiler backend
data BackendOpt = C | LLVM
  deriving (Show, Read, Eq, Ord)

-- Dump options
data DumpOpt = AST | PPR | LINT | RENAME | LIFT | TIM | CG
  deriving (Show, Read, Eq, Ord)
