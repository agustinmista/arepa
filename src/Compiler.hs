module Compiler 
  ( CompilerT
  , runCompilerT
  , Compiler
  , runCompiler
  , withCompilerT
  , lookupCompilerEnv
  , throwCompilerError
  , logCompilerMsg
  , getCompilerState
  , putCompilerState
  , emptyCompilerEnv
  , CompilerError(..)
  , CompilerEnv(..)
  , CompilerLog(..)
  , CompilerState(..)
  ) where

import Control.Monad.Except
import Control.Monad.RWS

-- Pass specific imports
import Text.Megaparsec (ParseErrorBundle)
import Data.Void
import Data.Text (Text)

----------------------------------------
-- Compiler monad
----------------------------------------

-- Monad transformer with global read-only environment and error throwing

newtype CompilerT m a = CompilerT (ExceptT CompilerError (RWST CompilerEnv CompilerLog CompilerState m) a)
  deriving (
    Functor, Applicative, Monad, 
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

-- Non-transformer synomym

type Compiler a = CompilerT IO a

runCompiler :: CompilerEnv -> Compiler a -> IO (Either CompilerError a, CompilerLog)
runCompiler = runCompilerT

-- Compiler manipulation

throwCompilerError :: Monad m => CompilerError -> CompilerT m a
throwCompilerError = throwError 

lookupCompilerEnv :: Monad m => (CompilerEnv -> a) -> CompilerT m a
lookupCompilerEnv = asks 

logCompilerMsg :: Monad m => CompilerMsg -> CompilerT m ()
logCompilerMsg msg = tell (CompilerLog [msg]) 

getCompilerState :: Monad m => CompilerT m CompilerState
getCompilerState = get

putCompilerState :: Monad m => CompilerState -> CompilerT m ()
putCompilerState = put

----------------------------------------
-- Compilation environment (read-only)
----------------------------------------

data CompilerEnv = CompilerEnv

emptyCompilerEnv :: CompilerEnv
emptyCompilerEnv = CompilerEnv

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

----------------------------------------
-- Compilation log (write-only)
----------------------------------------

newtype CompilerLog = CompilerLog [CompilerMsg]
  deriving Show
  deriving Semigroup via [CompilerMsg]
  deriving Monoid via [CompilerMsg]

newtype CompilerMsg = CompilerMsg ()
  deriving Show

----------------------------------------
-- Compilation errors
----------------------------------------

data CompilerError = 
    PsError (ParseErrorBundle Text Void) 
  | DsError DsError 
  | TcError TcError 
  | CgError CgError 
  deriving Show

data DsError = SomeDsError deriving Show
data TcError = SomeTcError deriving Show
data CgError = SomeCgError deriving Show