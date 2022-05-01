module Clang where
import System.Process
import System.FilePath
import System.Directory


import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

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

runClang :: Int -> Bool -> [FilePath] -> FilePath -> FilePath -> IO Text
runClang opt dbg extra input output = do
  args <- mkClangArgs opt dbg extra input output
  Text.pack <$> readProcess "clang" args []

mkClangArgs :: Int -> Bool -> [FilePath] -> FilePath -> FilePath -> IO [String]
mkClangArgs opt dbg extra input output = do
  c_files <- readRtsSrcDir
  return $
    c_files <>
    extra <>
    [ input
    , "-Wno-override-module"
    , "-I", rtsInclude
    , "-O" <> show opt
    , if dbg then "-DDEBUG" else ""
    , "-o", output
    ]

