module Language.Arepa.Compiler.IO where

import Control.Monad

import System.IO.Temp
import System.FilePath
import System.Process
import System.Directory

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Language.Arepa.Compiler.Monad

----------------------------------------
-- Input/output utilities
----------------------------------------

readArepaInput :: MonadArepa m => m Text
readArepaInput = do
  infile <- lookupCompilerOption optInput
  case infile of
    Nothing -> readStdin
    Just path -> readFromFile path

writeLLVMOutput :: MonadArepa m => Text -> m ()
writeLLVMOutput text = do
  llvmPath <- compiledLLVMPath
  writeToFile llvmPath text

----------------------------------------
-- Running clang
----------------------------------------

rtsSrc :: FilePath
rtsSrc = "rts" </> "src"

rtsInclude :: FilePath
rtsInclude = "rts" </> "include"

readRtsSrcDir :: IO [FilePath]
readRtsSrcDir = do
  files <- listDirectory rtsSrc
  let c_files = filter ((".c" ==) . takeExtension) files
  return [ rtsSrc </> c_file | c_file <- c_files ]

runClang :: MonadArepa m => [String] ->  m ()
runClang args = do
  whenVerbose $ dump "Calling clang with CLI argumnents" (prettyPrint args)
  stdout <- Text.pack <$> liftIO (readProcess "clang" args [])
  unless (Text.null stdout) $ do
    warning stdout

mkClangArgs :: MonadArepa m => m [String]
mkClangArgs = do
  rtsFiles   <- liftIO readRtsSrcDir
  extraFiles <- lookupCompilerOption optInclude
  optLevel   <- lookupCompilerOption optOptimize
  dbgFlag    <- lookupCompilerOption optDebug
  llvmPath   <- compiledLLVMPath
  binPath    <- compiledBinaryPath
  return $
    rtsFiles <>
    extraFiles <>
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

compiledLLVMPath :: MonadArepa m => m FilePath
compiledLLVMPath = do
  input <- lookupCompilerOption optInput
  case input of
    Nothing -> do
      liftIO $ emptyTempFile "." "arepa.ll"
    Just path -> do
      return (replaceExtension path "ll")

compiledBinaryPath :: MonadArepa m => m FilePath
compiledBinaryPath = do
  output <- lookupCompilerOption optOutput
  case output of
    Nothing -> do
      return "a.out"
    Just path -> do
      return path