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
  , CompilerMsg(..)
  , logCompilerMsg
  , compilerIO
  , prettyPrint
  , prettyShow
  , liftIO
  ) where

import System.IO

import Control.Monad.Except
import Control.Monad.Reader

import Data.Text.Lazy (Text)
import Data.Text.Lazy    qualified as Text
import Data.Text.Lazy.IO qualified as Text

import Prettyprinter
import Prettyprinter.Render.Text
import Text.Pretty.Simple


----------------------------------------
-- Compiler monad
----------------------------------------

-- Monad with exceptions and global read-only environment

newtype Compiler err opt a = Compiler (ExceptT err (ReaderT opt IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError err, MonadReader opt, MonadFix)

runCompiler :: opt -> Compiler err opt a -> IO (Either err a)
runCompiler opt (Compiler m) = runReaderT (runExceptT m) opt

-- The same as `runCompiler` but calls error if there are any unhandled exceptions
runCompiler' :: Pretty err => opt -> Compiler err opt a -> IO a
runCompiler' opt ma = do
  res <- runCompiler opt ma
  case res of
    Left err -> do
      Text.hPutStrLn stderr (prettyPrint err)
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
    MonadIO m,
    MonadFix m
  )

----------------------------------------
-- Error handling

throwCompilerError :: MonadCompiler err opt m => err -> m a
throwCompilerError = throwError

handleCompilerError :: MonadCompiler err opt m => (err -> m a) -> m a -> m a
handleCompilerError = flip catchError

printCompilerError :: (MonadCompiler err opt m, Pretty err) => err -> m ()
printCompilerError err = writeStderr (prettyPrint err)

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
  WarningMsg :: FilePath -> Text -> CompilerMsg
  DebugMsg   :: FilePath -> Text -> Maybe Text -> CompilerMsg


renderCompilerMsg :: CompilerMsg -> Text
renderCompilerMsg (WarningMsg path msg) =
  "[WARNING] " <> Text.pack path <> ": " <> msg
renderCompilerMsg (DebugMsg path msg mbobj) =
  "[DEBUG] "   <> Text.pack path <> ": " <> msg <> maybe "" (":\n" <>) mbobj


logCompilerMsg :: MonadCompiler err opt m => CompilerMsg -> m ()
logCompilerMsg msg = writeStderr (renderCompilerMsg msg)

----------------------------------------
-- Other utilities
----------------------------------------

prettyPrint :: Pretty a => a -> Text
prettyPrint a = renderLazy (layoutPretty defaultLayoutOptions (pretty a))

prettyShow :: Show a => a -> Text
prettyShow = pShowOpt defaultOutputOptionsDarkBg {
   outputOptionsCompact = True,
   outputOptionsCompactParens = True,
   outputOptionsIndentAmount = 2,
   outputOptionsPageWidth = 140
 }