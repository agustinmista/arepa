module Compiler 
  ( CompilerT
  , runCompilerT
  , Compiler
  , runCompiler
  , lookupCompilerEnv
  , throwCompilerError
  , logCompilerMsg
  , CompilerError(..)
  , CompilerEnv(..)
  , CompilerLog(..)
  ) where

import Control.Monad.Except
import Control.Monad.RWS

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

runCompilerT :: Monad m => CompilerEnv -> CompilerT m a -> m (Either CompilerError a, CompilerLog) 
runCompilerT env (CompilerT m) = do 
  (res, _, log) <- runRWST (runExceptT m) env initCompilerState
  case res of
    Left ce -> return (Left ce, log)
    Right a -> return (Right a, log)

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

----------------------------------------
-- Compilation environment (read-only)
----------------------------------------

data CompilerEnv = CompilerEnv

----------------------------------------
-- Compilation state (read-write)
----------------------------------------

data CompilerState = CompilerState

initCompilerState :: CompilerState
initCompilerState = CompilerState

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
    PsError PsError 
  | DsError DsError 
  | TcError TcError 
  | CgError CgError 
  deriving Show

data PsError = SomePsError deriving Show 
data DsError = SomeDsError deriving Show
data TcError = SomeTcError deriving Show
data CgError = SomeCgError deriving Show