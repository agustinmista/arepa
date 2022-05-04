module Language.Arepa.Compiler.Interpret
  ( interpretCodeStore
  ) where

import Data.Maybe

import Language.TIM

import Language.Arepa.Compiler.Monad

----------------------------------------
-- Interpretion of translated modules
----------------------------------------

-- Interpret a TIM code store, invoking some function
interpretCodeStore :: MonadArepa m => CodeStore -> m [Value]
interpretCodeStore store = do
  entry <- lookupCompilerOption optEntryPoint
  let fun = mkName (fromMaybe "main" entry)
  whenVerbose $ debug ("Interpreting code store [entry=" <> prettyPrint fun <> "]")
  (res, trace) <- liftIO $ do
    runTIM store $ invokeFunction fun []
  whenVerbose $ dump "Interpreter intermediate states" (prettyPrint trace)
  case res of
    Left err -> throwInterpreterError err
    Right vals -> return vals
