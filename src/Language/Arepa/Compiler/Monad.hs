module Language.Arepa.Compiler.Monad
  ( CompilerT
  , runCompilerT
  , Compiler
  , runCompiler
  , withCompilerT
  -- Compilation errors
  , MonadError
  , CompilerError
  , throwCompilerError
  , ParserError
  , throwParserError
  , CodegenError
  , throwCodegenError
  -- Read-only environment
  , MonadReader
  , CompilerEnv
  , emptyCompilerEnv
  , mkCompilerEnv
  , lookupCliOpt
  -- Write-only log
  , MonadWriter
  , CompilerLog
  , renderCompilerLog
  , logWarningMsg
  , logDebugMsg
  -- Other utilities
  , liftIO
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

-- Pass specific imports
import Text.Megaparsec (ParseErrorBundle)
import Data.Void

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import Language.Arepa.Compiler.CLI

----------------------------------------
-- Compiler monad
----------------------------------------

-- Monad transformer with global read-only environment and error throwing

newtype CompilerT m a = CompilerT (ExceptT CompilerError (ReaderT CompilerEnv (WriterT CompilerLog m)) a)
  deriving (
    Functor, Applicative, Monad, MonadIO,
    MonadError CompilerError,
    MonadReader CompilerEnv,
    MonadWriter CompilerLog
  )

instance MonadTrans CompilerT where
  lift = CompilerT . lift . lift . lift

runCompilerT :: Monad m => CompilerEnv -> CompilerT m a -> m (Either CompilerError a, CompilerLog)
runCompilerT env (CompilerT m) = do
  (res, msgs) <- runWriterT (runReaderT (runExceptT m) env)
  case res of
    Left ce -> return (Left ce, msgs)
    Right a -> return (Right a, msgs)

-- Non-transformer synomym

type Compiler a = CompilerT IO a

runCompiler :: CompilerEnv -> Compiler a -> IO (Either CompilerError a, CompilerLog)
runCompiler = runCompilerT

-- Map the inner computation using a given function
withCompilerT :: (m (Either CompilerError a, CompilerLog) ->
                  n (Either CompilerError b, CompilerLog))
             -> CompilerT m a
             -> CompilerT n b
withCompilerT f (CompilerT ex) =
  CompilerT $
    flip mapExceptT ex $ \rdr ->
    flip mapReaderT rdr $ \wtr ->
    flip mapWriterT wtr $ \m ->
      f m

----------------------------------------
-- Compilation environment (read-only)
----------------------------------------

data CompilerEnv = CompilerEnv {
  ce_cli_opts :: CliOpts
}

emptyCompilerEnv :: CompilerEnv
emptyCompilerEnv = CompilerEnv {
  ce_cli_opts = defaultCliOpts
}

mkCompilerEnv :: CliOpts -> CompilerEnv
mkCompilerEnv opts = CompilerEnv {
  ce_cli_opts = opts
}

-- Compiler utilities

lookupCliOpt :: MonadReader CompilerEnv m => (CliOpts -> a) -> m a
lookupCliOpt f = asks (f . ce_cli_opts)

----------------------------------------
-- Compilation log (write-only)
----------------------------------------

newtype CompilerLog = CompilerLog [CompilerMsg]
  deriving Show
  deriving Semigroup via [CompilerMsg]
  deriving Monoid    via [CompilerMsg]

renderCompilerLog :: CompilerLog -> Text
renderCompilerLog (CompilerLog msgs) =
  Text.unlines (renderCompilerMsg <$> msgs)

data CompilerMsg =
    WarningMsg Text
  | DebugMsg Text
  deriving Show

renderCompilerMsg :: CompilerMsg -> Text
renderCompilerMsg (WarningMsg msg) =
  "[WARNING] " <> msg
renderCompilerMsg (DebugMsg msg) =
  "[DEBUG] " <> msg

-- Compiler utilities

logCompilerMsg :: MonadWriter CompilerLog m => CompilerMsg -> m ()
logCompilerMsg msg = tell (CompilerLog [msg])

logWarningMsg :: MonadWriter CompilerLog m => Text -> m ()
logWarningMsg = logCompilerMsg . WarningMsg

logDebugMsg :: MonadWriter CompilerLog m => Text -> m ()
logDebugMsg = logCompilerMsg . DebugMsg

----------------------------------------
-- Compilation errors
----------------------------------------

data CompilerError =
    ParserError ParserError
  | CodegenError CodegenError
  deriving Show

-- Compiler utilities

throwCompilerError :: MonadError CompilerError m => CompilerError -> m a
throwCompilerError = throwError

-- Parse errors
type ParserError = ParseErrorBundle Text Void

throwParserError :: MonadError CompilerError m => ParserError -> m a
throwParserError err = throwCompilerError (ParserError err)

-- Other errors (to be completed)
type CodegenError = Text

throwCodegenError :: MonadError CompilerError m => CodegenError -> m a
throwCodegenError err = throwCompilerError (CodegenError err)