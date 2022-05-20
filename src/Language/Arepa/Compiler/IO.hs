module Language.Arepa.Compiler.IO where

import Control.Monad

import System.FilePath
import System.Process
import System.Directory

import Data.Maybe

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Language.Arepa.Compiler.Monad
import Language.TIM.Syntax

----------------------------------------
-- Input/output utilities
----------------------------------------

readArepaSourceFile :: MonadArepa m => m Text
readArepaSourceFile = do
  infile <- lookupCompilerOption optInput
  case infile of
    Nothing -> do
      whenVerbose $ debug "Reading arepa source from stdin"
      readStdin
    Just path -> do
      whenVerbose $ debug ("Reading arepa source file " <> prettyPrint path)
      readFromFile path

readExtraTIMCodeStores :: MonadArepa m => m [CodeStore]
readExtraTIMCodeStores = do
  extraFiles <- lookupCompilerOption optInclude
  forM extraFiles $ \extraFile -> do
    let timPath = mkTIMFilePath extraFile
    whenVerbose $ debug ("Loading TIM code file " <> prettyPrint timPath)
    text <- readFromFile timPath
    return (read (Text.unpack text))

writeTIMCodeStore :: MonadArepa m => CodeStore -> m ()
writeTIMCodeStore store = do
  timPath <- compiledTIMPath
  whenVerbose $ debug ("Writing compiled TIM code to " <> prettyPrint timPath)
  writeToFile timPath (Text.pack (show store))

writeLLVMModule :: MonadArepa m => Text -> m ()
writeLLVMModule text = do
  llvmPath <- compiledLLVMPath
  whenVerbose $ debug ("Writing compiled llvm to " <> prettyPrint llvmPath)
  writeToFile llvmPath text

----------------------------------------
-- Running clang
----------------------------------------

rtsSrc :: FilePath
rtsSrc = "rts" </> "src"

rtsInclude :: FilePath
rtsInclude = "rts" </> "include"

readRtsSrcDir :: MonadArepa m => m [FilePath]
readRtsSrcDir = do
  files <- liftIO (listDirectory rtsSrc)
  let c_files = filter ((".c" ==) . takeExtension) files
  whenVerbose $ dump "Found RTS source files" c_files
  return [ rtsSrc </> c_file | c_file <- c_files ]

runClang :: MonadArepa m => [String] ->  m ()
runClang args = do
  whenVerbose $ dump "Calling clang with CLI argumnents" args
  stdout <- Text.pack <$> liftIO (readProcess "clang" args [])
  unless (Text.null stdout) $ do
    warning stdout

mkClangArgs :: MonadArepa m => m [String]
mkClangArgs = do
  rtsFiles   <- readRtsSrcDir
  extraFiles <- lookupCompilerOption optInclude
  optLevel   <- lookupCompilerOption optOptimize
  dbgFlag    <- lookupCompilerOption optDebug
  llvmPath   <- compiledLLVMPath
  binPath    <- compiledBinaryPath
  return $
    rtsFiles <>
    (mkLLVMFilePath <$> extraFiles) <>
    [ "-DDEBUG" | dbgFlag ] <>
    [ llvmPath
    , "-Wno-override-module"
    , "-I", rtsInclude
    , "-O" <> show optLevel
    , "-o", binPath
    ]

----------------------------------------
-- File path manipulations
----------------------------------------

mkLLVMFilePath :: FilePath -> FilePath
mkLLVMFilePath = flip replaceExtension "ll"

mkTIMFilePath :: FilePath -> FilePath
mkTIMFilePath = flip replaceExtension "tim"

compiledLLVMPath :: MonadArepa m => m FilePath
compiledLLVMPath = compiledSourceFilePath mkLLVMFilePath

compiledTIMPath :: MonadArepa m => m FilePath
compiledTIMPath  = compiledSourceFilePath mkTIMFilePath

compiledSourceFilePath :: MonadArepa m => (FilePath -> FilePath) -> m FilePath
compiledSourceFilePath f = do
  input <- lookupCompilerOption optInput
  return (f (fromMaybe "stdin" input))

compiledBinaryPath :: MonadArepa m => m FilePath
compiledBinaryPath = do
  output <- lookupCompilerOption optOutput
  return (fromMaybe "a.out" output)