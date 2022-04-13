module Compiler 
  ( CompilerT
  , runCompilerT
  , Compiler
  , runCompiler
  , withCompilerT
  -- Read-only environment
  , CompilerEnv
  , emptyCompilerEnv
  , mkCompilerEnv
  , lookupCliOpt
  -- Write-only log
  , CompilerLog
  , logCompilerMsg
  -- Internal state
  , CompilerState
  , getCurrentFile
  , setCurrentFile
  -- Compilation errors
  , CompilerError
  , throwCompilerError
  , ParserError
  , throwParserError
  -- Other utilities
  , liftIO
  ) where


import Control.Monad.Except
import Control.Monad.RWS

-- Pass specific imports
import Text.Megaparsec (ParseErrorBundle)
import Data.Void
import Data.Text.Lazy (Text)

import CLI

----------------------------------------
-- Compiler monad
----------------------------------------

-- Monad transformer with global read-only environment and error throwing

newtype CompilerT m a = CompilerT (ExceptT CompilerError (RWST CompilerEnv CompilerLog CompilerState m) a)
  deriving (
    Functor, Applicative, Monad, MonadIO, 
    MonadError CompilerError, 
    MonadReader CompilerEnv,
    MonadWriter CompilerLog,
    MonadState CompilerState
  )

instance MonadTrans CompilerT where
  lift = CompilerT . lift . lift

runCompilerT :: Monad m => CompilerEnv -> CompilerT m a -> m (Either CompilerError a, CompilerLog) 
runCompilerT env (CompilerT m) = do 
  (res, _, msgs) <- runRWST (runExceptT m) env initCompilerState
  case res of
    Left ce -> return (Left ce, msgs)
    Right a -> return (Right a, msgs)

-- Non-transformer synomym

type Compiler a = CompilerT IO a

runCompiler :: CompilerEnv -> Compiler a -> IO (Either CompilerError a, CompilerLog)
runCompiler = runCompilerT

-- Map the inner computation using a given function
withCompilerT :: (m (Either CompilerError a, CompilerState, CompilerLog) -> 
                  n (Either CompilerError b, CompilerState, CompilerLog)) 
             -> CompilerT m a 
             -> CompilerT n b
withCompilerT f (CompilerT ex) = 
  CompilerT $ 
    flip mapExceptT ex $ \rwst -> 
    flip mapRWST rwst $ \m ->
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

lookupCliOpt :: Monad m => (CliOpts -> a) -> CompilerT m a
lookupCliOpt f = f <$> asks ce_cli_opts 

----------------------------------------
-- Compilation state (read-write)
----------------------------------------

data CompilerState = CompilerState { 
  cs_curr_file :: Maybe FilePath 
}

initCompilerState :: CompilerState
initCompilerState = CompilerState { 
  cs_curr_file = Nothing 
}

-- Compiler utilities

getCurrentFile :: Monad m => CompilerT m (Maybe FilePath)
getCurrentFile = cs_curr_file <$> get

setCurrentFile :: Monad m => FilePath -> CompilerT m ()
setCurrentFile path = modify' $ \cs ->
   cs { cs_curr_file = Just path } 

----------------------------------------
-- Compilation log (write-only)
----------------------------------------

newtype CompilerLog = CompilerLog [CompilerMsg]
  deriving Show
  deriving Semigroup via [CompilerMsg]
  deriving Monoid via [CompilerMsg]

newtype CompilerMsg = CompilerMsg ()
  deriving Show

-- Compiler utilities

logCompilerMsg :: Monad m => CompilerMsg -> CompilerT m ()
logCompilerMsg msg = tell (CompilerLog [msg]) 

----------------------------------------
-- Compilation errors
----------------------------------------

data CompilerError = 
    ParserError ParserError 
  | OtherError OtherError
  deriving Show

-- Compiler utilities

throwCompilerError :: Monad m => CompilerError -> CompilerT m a
throwCompilerError = throwError 

-- Parse errors
type ParserError = ParseErrorBundle Text Void 

throwParserError :: Monad m => ParserError -> CompilerT m a
throwParserError err = throwCompilerError (ParserError err)

-- Other errors (to be completed)
type OtherError = Void