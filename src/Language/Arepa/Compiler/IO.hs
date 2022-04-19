module Language.Arepa.Compiler.IO where

import Language.Arepa.Compiler.Monad


----------------------------------------
-- Input/output utilities
----------------------------------------

readInput :: MonadArepa m => m Text
readInput = do
  infile <- lookupCompilerOption optInput
  case infile of
    Nothing -> readStdin
    Just path -> readFromFile path

writeOutput :: MonadArepa m => Text -> m ()
writeOutput text = do
  outfile <- lookupCompilerOption optOutput
  case outfile of
    Nothing -> writeStdout text
    Just path -> writeToFile path text
