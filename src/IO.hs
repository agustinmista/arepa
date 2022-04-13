module IO where

import System.IO

import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO qualified as Text

import CLI
import Compiler

----------------------------------------
-- Input output utilities
----------------------------------------

readInput :: Compiler Text
readInput = do 
  infile <- lookupCliOpt optInput
  case infile of
    Nothing -> readStdin
    Just path -> readFromFile path

writeOutput :: Text -> Compiler ()
writeOutput text = do
  outfile <- lookupCliOpt optOutput
  case outfile of
    Nothing -> writeStdout text
    Just path -> writeToFile path text
  

----------------------------------------
-- IO primitives

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