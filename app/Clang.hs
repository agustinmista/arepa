module Clang where

import GHC.IO.Handle
import System.IO.Temp
import System.Process
import System.FilePath
import System.Directory

import Data.Maybe

import Data.Text.Lazy (Text)
import Data.Text.Lazy    qualified as Text
import Data.Text.Lazy.IO qualified as Text

----------------------------------------
-- Running clang
----------------------------------------

compileAndLinkLLVM :: Int -> Text -> Maybe FilePath -> Maybe FilePath -> IO Text
compileAndLinkLLVM opt llvm mbinput mboutput = do
  let binpath = fromMaybe "a.out" mboutput
  case mbinput of
    Nothing -> do
      withTempFile "." "arepa_temp.ll" $ \llpath llhandle -> do
        Text.hPutStrLn llhandle llvm
        hFlush llhandle
        runClang opt llpath binpath
    Just path -> do
      let llpath = replaceExtension path "ll"
      Text.writeFile llpath llvm
      runClang opt llpath binpath

rtsSrc :: FilePath
rtsSrc = "rts" </> "src"

rtsInclude :: FilePath
rtsInclude = "rts" </> "include"

readRtsSrcDir :: IO [FilePath]
readRtsSrcDir = do
  files <- listDirectory rtsSrc
  let c_files = filter ((".c" ==) . takeExtension) files
  return [ rtsSrc </> c_file | c_file <- c_files ]

runClang :: Int -> FilePath -> FilePath -> IO Text
runClang opt input output = do
  args <- mkClangArgs opt input output
  Text.pack <$> readProcess "clang" args []

mkClangArgs :: Int -> FilePath -> FilePath -> IO [String]
mkClangArgs opt input output = do
  c_files <- readRtsSrcDir
  return $
    c_files <>
    [ input
    , "-Wno-override-module"
    , "-I", rtsInclude
    , "-O" <> show opt
    , "-o", output
    ]
