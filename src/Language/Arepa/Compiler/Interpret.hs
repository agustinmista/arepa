module Language.Arepa.Compiler.Interpret
  ( interpretCodeStore
  ) where

import Data.Maybe

import Language.TIM

import Language.Arepa.Compiler.Monad

----------------------------------------
-- Interpretion of translated modules
----------------------------------------

-- Interpret a TIM code store, invoking the entry point function
interpretCodeStore :: MonadArepa m => CodeStore -> m ()
interpretCodeStore store = do
  entry <- lookupCompilerOption optEntryPoint
  stdin <- lookupCompilerOption optInterpretStdin
  stdout <- lookupCompilerOption optInterpretStdout
  let fun = mkName (fromMaybe "main" entry)
  whenVerbose $ debug ("Interpreting code store [entry=" <> prettyPrint fun <> "]")
  (res, trace) <- liftIO $ runTIM stdin stdout store $ invokeFunction fun []
  whenVerbose $ dump "Interpreter intermediate states" (prettyPrint trace)
  case res of
    Left err -> do
      throwInterpreterError err
    Right vals -> do
      whenVerbose $ dump "Final value stack" (prettyPrint vals)
