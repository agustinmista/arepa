module Language.Arepa.Compiler.Interpret
  ( interpretCodeStore
  ) where

import Language.TIM

import Language.Arepa.Compiler.Monad
import Language.Arepa.Compiler.IO

----------------------------------------
-- Interpretion of translated modules
----------------------------------------

-- Interpret a TIM code store, invoking the entry point function
interpretCodeStore :: MonadArepa m => CodeStore -> m ()
interpretCodeStore store = do
  entry <- lookupCompilerOption optEntryPoint
  interactive <- lookupCompilerOption optInteractive
  stdin <- lookupCompilerOption optInterpretStdin
  stdout <- lookupCompilerOption optInterpretStdout
  extraStores <- readExtraTIMCodeStores
  let fun = mkName entry
  let stores = store : extraStores
  whenVerbose $ debug ("Interpreting code store [entry=" <> prettyPrint fun <> "]")
  (res, trace) <- liftIO $ runTIM interactive stdin stdout stores $ invokeFunction fun []
  whenVerbose $ dump "Interpreter intermediate states" (prettyPrint trace)
  case res of
    Left err -> do
      throwInterpreterError err
    Right vals -> do
      whenVerbose $ dump "Final value stack" vals
