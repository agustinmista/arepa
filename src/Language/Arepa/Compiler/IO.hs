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
  path <- lookupCompilerOption optInput
  whenVerbose $ debug ("Reading arepa source file " <> prettyPrint path)
  readFromFile path

readExtraTIMCodeStores :: MonadArepa m => m [CodeStore]
readExtraTIMCodeStores = do
  extraFiles <- lookupCompilerOption optInclude
  forM extraFiles $ \extraFile -> do
    let timPath = mkTIMFilePath extraFile
    whenVerbose $ debug ("Loading TIM code file " <> prettyPrint timPath)
    text <- readFromFile timPath
    return (decodeCodeStore text)

writeCompiledFile :: MonadArepa m => FilePath -> Text -> m ()
writeCompiledFile path text = do
  whenVerbose $ debug ("Writing compiled file " <> prettyPrint path)
  writeToFile path text

----------------------------------------
-- Running clang
----------------------------------------

rtsSourceDir :: FilePath
rtsSourceDir = "rts" </> "src"

rtsIncludeDir :: FilePath
rtsIncludeDir = "rts" </> "include"

readRTSSourceDir :: MonadArepa m => m [FilePath]
readRTSSourceDir = do
  files <- liftIO (listDirectory rtsSourceDir)
  let c_files = filter ((".c" ==) . takeExtension) files
  whenVerbose $ dump "Found RTS source files" c_files
  return [ rtsSourceDir </> c_file | c_file <- c_files ]

runClang :: MonadArepa m => [String] ->  m ()
runClang args = do
  whenVerbose $ dump "Calling clang with CLI argumnents" args
  stdout <- Text.pack <$> liftIO (readProcess "clang" args [])
  unless (Text.null stdout) $ do
    warning stdout

mkClangArgs :: MonadArepa m => m [String]
mkClangArgs = do
  -- RTS stuff
  rtsSourceFiles <- readRTSSourceDir
  let rtsIncludeFlags = [ "-I", rtsIncludeDir ]
  -- Optimization flags
  optLevel <- lookupCompilerOption optOptimize
  let optFlag = [ "-O" <> show optLevel ]
  -- Binary debug flags
  dbgEnabled <- lookupCompilerOption optDebug
  let dbgFlag = [ "-DDEBUG" | dbgEnabled ]
  -- Binary output flags
  binPath <- compiledBinaryPath
  let binOutputFlag = [ "-o", binPath ]
  -- Current file
  headerFile <- compiledHPath
  let includeFlag = [ "-I", headerFile ]
  sourceFile <- compiledCPath
  -- Extra files
  extraFiles <- lookupCompilerOption optInclude
  let extraSourceFiles = fmap mkCFilePath extraFiles
  let extraIncludeFlags = [ "-I." | not (null extraFiles) ]
  return $
    -- Source files
    rtsSourceFiles <> [ sourceFile ] <> extraSourceFiles <>
    -- Include flags
    rtsIncludeFlags <> includeFlag <> extraIncludeFlags <>
    -- Other flags
    dbgFlag <> optFlag <>
    -- Output flag
    binOutputFlag

----------------------------------------
-- File path manipulations
----------------------------------------

-- Changing file extensions

mkArepaFilePath :: FilePath -> FilePath
mkArepaFilePath = flip replaceExtension "ar"

mkHFilePath :: FilePath -> FilePath
mkHFilePath = flip replaceExtension "h"

mkCFilePath :: FilePath -> FilePath
mkCFilePath = flip replaceExtension "c"

mkLLVMFilePath :: FilePath -> FilePath
mkLLVMFilePath = flip replaceExtension "ll"

mkTIMFilePath :: FilePath -> FilePath
mkTIMFilePath = flip replaceExtension "tim"

-- Returning the filepaths associated to the current target file

compiledHPath :: MonadArepa m => m FilePath
compiledHPath = compiledSourceFilePath mkHFilePath

compiledCPath :: MonadArepa m => m FilePath
compiledCPath = compiledSourceFilePath mkCFilePath

compiledLLVMPath :: MonadArepa m => m FilePath
compiledLLVMPath = compiledSourceFilePath mkLLVMFilePath

compiledTIMPath :: MonadArepa m => m FilePath
compiledTIMPath  = compiledSourceFilePath mkTIMFilePath

compiledSourceFilePath :: MonadArepa m => (FilePath -> FilePath) -> m FilePath
compiledSourceFilePath f = do
  f <$> lookupCompilerOption optInput

compiledBinaryPath :: MonadArepa m => m FilePath
compiledBinaryPath = do
  fromMaybe "a.out" <$> lookupCompilerOption optOutput