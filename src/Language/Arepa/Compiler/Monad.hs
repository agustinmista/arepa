module Language.Arepa.Compiler.Monad
  ( module Language.Arepa.Compiler.Monad
  , module Control.Monad.Compiler
  , module Language.Arepa.Compiler.Error
  , module Language.Arepa.Compiler.Options
  ) where

import Control.Monad.Extra
import Control.Monad.Compiler

import Data.Text.Lazy (Text)

import Language.Arepa.Compiler.Error
import Language.Arepa.Compiler.Options

----------------------------------------
-- Compiler monad
----------------------------------------

-- A concrete instance of `Compiler` with tasty errors and options.
type Arepa a = Compiler ArepaError ArepaOpts a

-- Specialized versions of `runCompiler` and `runCompiler'`

runArepa :: ArepaOpts -> Arepa a -> IO (Either ArepaError a)
runArepa = runCompiler

runArepa' :: ArepaOpts -> Arepa a -> IO a
runArepa' = runCompiler'

-- Test a computation with default options
testArepa :: Arepa a -> IO a
testArepa = runCompiler' defaultOpts

-- A concete instantiation of `MonadCompiler` with tasty errors and options.
type MonadArepa m = MonadCompiler ArepaError ArepaOpts m


----------------------------------------
-- Compilation messages

warning :: MonadArepa m => Text -> m ()
warning msg = do
  logCompilerMsg (WarningMsg msg)

debug :: MonadArepa m => Text -> m ()
debug msg = do
  whenM (lookupCompilerOption optVerbose) $ do
    logCompilerMsg (DebugMsg msg)

dump :: MonadArepa m => Text -> Text -> m ()
dump msg obj = logCompilerMsg (DumpMsg msg obj)

----------------------------------------
-- Compilation errors

throwParserError :: MonadArepa m => ParserError -> m a
throwParserError err = throwCompilerError (ParserError err)

throwInterpreterError :: MonadArepa m => InterpreterError -> m a
throwInterpreterError err = throwCompilerError (InterpreterError err)

throwInternalError :: MonadArepa m => InternalError -> m a
throwInternalError err = throwCompilerError (InternalError err)

notImplemented :: MonadArepa m => InternalError -> m a
notImplemented desc = throwInternalError (desc <> ": not yet implemented")

----------------------------------------
-- Compiler options

hasDumpEnabled :: MonadArepa m => DumpOpt -> m Bool
hasDumpEnabled opt = (opt `elem`) <$> lookupCompilerOption optDump

hasVerboseEnabled :: MonadArepa m => m Bool
hasVerboseEnabled = lookupCompilerOption optVerbose

hasInterpretEnabled :: MonadArepa m => m Bool
hasInterpretEnabled = lookupCompilerOption optInterpret