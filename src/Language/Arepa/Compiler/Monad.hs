module Language.Arepa.Compiler.Monad
  ( module Language.Arepa.Compiler.Monad
  , module Control.Monad.Compiler
  , module Language.Arepa.Compiler.Error
  , module Language.Arepa.Compiler.Options
  ) where

import Control.Monad.Extra
import Control.Monad.Compiler

import Data.Maybe

import Data.Text.Lazy (Text)

import Prettyprinter

import Language.Arepa.Compiler.Error
import Language.Arepa.Compiler.Options

import Language.TIM.Interpreter

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

-- Log a warning message
warning :: MonadArepa m => Text -> m ()
warning msg = do
  path <- getInputPath
  logCompilerMsg (WarningMsg path msg)

-- Log a debug message
debug :: MonadArepa m => Text -> m ()
debug msg = do
  path <- getInputPath
  logCompilerMsg (DebugMsg path msg Nothing)

-- Log a debug message along with some dump values
dump :: (MonadArepa m, Pretty a) => Text -> a -> m ()
dump msg obj = do
  path <- getInputPath
  logCompilerMsg (DebugMsg path msg (Just (prettyPrint obj)))

-- Report a message only when in debug mode (-v/--verbose)
whenVerbose :: MonadArepa m => m () -> m ()
whenVerbose = whenM hasVerboseEnabled

-- Report a message only when a certain dump flag is enabled
whenDump :: MonadArepa m => DumpOpt -> m () -> m ()
whenDump flag = whenM (hasDumpEnabled flag)

----------------------------------------
-- Compilation errors

throwParserError :: MonadArepa m => ParserError -> m a
throwParserError err = throwCompilerError (ParserError err)

throwLinterError :: MonadArepa m => Text -> m a
throwLinterError err = do
  path <- getInputPath
  throwCompilerError (LinterError (path, err))

throwRenamerError :: MonadArepa m => Text -> m a
throwRenamerError err = do
  path <- getInputPath
  throwCompilerError (RenamerError (path, err))

throwInterpreterError :: MonadArepa m => TIMError -> m a
throwInterpreterError err = do
  path <- getInputPath
  throwCompilerError (InterpreterError (path, err))

throwInternalError :: MonadArepa m => Text -> m a
throwInternalError err = do
  path <- getInputPath
  throwCompilerError (InternalError (path, err))

notImplemented :: MonadArepa m => Text -> m a
notImplemented desc = throwInternalError (desc <> ": not yet implemented")

----------------------------------------
-- Compiler options

hasLinkingDisabled :: MonadArepa m => m Bool
hasLinkingDisabled = lookupCompilerOption optNoLinking

hasEmitMainEnabled :: MonadArepa m => m Bool
hasEmitMainEnabled = lookupCompilerOption optEmitMain

hasDumpEnabled :: MonadArepa m => DumpOpt -> m Bool
hasDumpEnabled opt = (opt `elem`) <$> lookupCompilerOption optDump

hasVerboseEnabled :: MonadArepa m => m Bool
hasVerboseEnabled = lookupCompilerOption optVerbose

hasInterpretEnabled :: MonadArepa m => m Bool
hasInterpretEnabled = lookupCompilerOption optInterpret

hasStrictEnabled :: MonadArepa m => m Bool
hasStrictEnabled = lookupCompilerOption optStrict

----------------------------------------
-- Utilities

getInputPath :: MonadArepa m => m FilePath
getInputPath = fromMaybe "<stdin>" <$> lookupCompilerOption optInput
