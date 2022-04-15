module Main where

import System.IO
import System.Exit

import Data.Text.Lazy (Text)
import Data.Text.Lazy    qualified as Text
import Data.Text.Lazy.IO qualified as Text

import CLI
import Compiler
import Parser
import TypeCheck
import CodeGen

----------------------------------------
-- Entry point
----------------------------------------

main :: IO ()
main = do
  opts <- parseCliOpts
  let env = mkCompilerEnv opts
  (res, msgs) <- runCompiler env compile
  print msgs
  case res of
    Left  ce -> print ce
    Right () -> exitSuccess

-- The compilation pipeline

compile :: Compiler ()
compile = readInput
      >>= parseFileModule
      >>= typeCheckModule
      >>= emitLLVM
      >>= renderLLVM
      >>= writeOutput

readInput :: Compiler Text
readInput = do
  return undefined

writeOutput :: Text -> Compiler ()
writeOutput _ = return ()

----------------------------------------
-- IO utilities

readStdin :: Compiler Text
readStdin = liftIO Text.getContents

readFromFile :: FilePath -> Compiler Text
readFromFile path = liftIO (Text.readFile path)

writeHandle :: Handle -> Text -> Compiler ()
writeHandle h text = liftIO (Text.hPutStrLn h text >> hFlush h)

writeStderr :: Text -> Compiler ()
writeStderr = writeHandle stderr

writeStdout :: Text -> Compiler ()
writeStdout = writeHandle stdout

writeToFile :: FilePath -> Text -> Compiler ()
writeToFile path = liftIO . Text.writeFile path