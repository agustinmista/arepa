module Control.Monad.Compiler
  ( Compiler
  , runCompiler
  , runCompiler'
  , MonadCompiler
  , throwCompilerError
  , handleCompilerError
  , printCompilerError
  , lookupCompilerOption
  , readStdin
  , readFromFile
  , writeStderr
  , writeStdout
  , writeToFile
  , warningMsg
  , debugMsg
  , compilerIO
  ) where

import System.IO

import Control.Monad.Except
import Control.Monad.Reader

import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO qualified as Text

import Prettyprinter
import Prettyprinter.Render.Text


----------------------------------------
-- Compiler monad
----------------------------------------

-- Monad transformer with exceptions, global read-only environment and write-only log

newtype Compiler err opt a = Compiler (ExceptT err (ReaderT opt IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError err, MonadReader opt)

runCompiler :: opt -> Compiler err opt a -> IO (Either err a)
runCompiler opt (Compiler m) = runReaderT (runExceptT m) opt

-- The same as `runCompiler` but calls error if there are any unhandled exceptions
runCompiler' :: Pretty err => opt -> Compiler err opt a -> IO a
runCompiler' opt ma = do
  res <- runCompiler opt ma
  case res of
    Left err -> do
      Text.hPutStrLn stderr (renderPpr err)
      hFlush stderr
      error "Unhandled exception"
    Right a -> return a

----------------------------------------
-- Compiler monad constraint

-- This constraint synonym let us write:
--   `foo :: MonadCompiler err opt m => m ()`
-- Instead of carrying all the constraints individually.

type MonadCompiler err opt m = (
    MonadError err m,
    MonadReader opt m,
    MonadIO m
  )

----------------------------------------
-- Error handling

throwCompilerError :: MonadCompiler err opt m => err -> m a
throwCompilerError = throwError

handleCompilerError :: MonadCompiler err opt m => (err -> m a) -> m a -> m a
handleCompilerError = flip catchError

printCompilerError :: (MonadCompiler err opt m, Pretty err) => err -> m ()
printCompilerError err = writeStderr (renderPpr err)

----------------------------------------
-- Option lookup

lookupCompilerOption :: MonadCompiler err opt m => (opt -> a) -> m a
lookupCompilerOption = asks

----------------------------------------
-- Compiler IO
----------------------------------------

compilerIO :: MonadCompiler err opt m => IO a -> m a
compilerIO = liftIO

readStdin :: MonadCompiler err opt m => m Text
readStdin = compilerIO Text.getContents

readFromFile :: MonadCompiler err opt m => FilePath -> m Text
readFromFile path = compilerIO (Text.readFile path)

writeHandle :: MonadCompiler err opt m  => Handle -> Text -> m ()
writeHandle h text = compilerIO (Text.hPutStrLn h text >> hFlush h)

writeStderr :: MonadCompiler err opt m => Text -> m ()
writeStderr = writeHandle stderr

writeStdout :: MonadCompiler err opt m => Text -> m ()
writeStdout = writeHandle stdout

writeToFile :: MonadCompiler err opt m => FilePath -> Text -> m ()
writeToFile path = compilerIO . Text.writeFile path

-- Compiler messages (written to stderr)

data CompilerMsg where
  WarningMsg :: Pretty a => Text -> Maybe a -> CompilerMsg
  DebugMsg   :: Pretty a => Text -> Maybe a -> CompilerMsg


renderCompilerMsg :: CompilerMsg -> Text
renderCompilerMsg (WarningMsg msg obj) =
  "[WARNING] " <> msg <> maybe mempty (\x -> ":\n" <> renderPpr x) obj
renderCompilerMsg (DebugMsg   msg obj) =
  "[DEBUG] "   <> msg <> maybe mempty (\x -> ":\n" <> renderPpr x) obj

logCompilerMsg :: MonadCompiler err opt m => CompilerMsg -> m ()
logCompilerMsg msg = writeStderr (renderCompilerMsg msg)


warningMsg :: (MonadCompiler err opt m, Pretty a) => Text -> Maybe a -> m ()
warningMsg msg obj = logCompilerMsg (WarningMsg msg obj)

debugMsg :: (MonadCompiler err opt m, Pretty a) => Text -> Maybe a -> m ()
debugMsg msg obj = logCompilerMsg (DebugMsg msg obj)

----------------------------------------
-- Other utilities
----------------------------------------

renderPpr :: Pretty a => a -> Text
renderPpr a = renderLazy (layoutPretty defaultLayoutOptions (pretty a))