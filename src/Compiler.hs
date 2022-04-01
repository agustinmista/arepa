module Compiler 
  ( CompilerT
  , runCompilerT
  , Compiler
  , runCompiler
  , compilerEnv
  , compilerError
  , CompilerEnv(..)
  , CompilerError(..)
  ) where

import Control.Monad.Reader
import Control.Monad.Except

----------------------------------------
-- Compiler monad
----------------------------------------

-- Monad transformer with global read-only environment and error throwing

newtype CompilerT m a = CompilerT (ReaderT CompilerEnv (ExceptT CompilerError m) a)
  deriving (Functor, Applicative, Monad, MonadReader CompilerEnv, MonadError CompilerError)

runCompilerT :: Monad m => CompilerEnv -> CompilerT m a -> m (Either CompilerError a) 
runCompilerT env (CompilerT m) = runExceptT (runReaderT m env)

-- Non-transformer synomym

type Compiler a = CompilerT IO a

runCompiler :: CompilerEnv -> Compiler a -> IO (Either CompilerError a)
runCompiler = runCompilerT

-- Compiler manipulation

compilerEnv :: Monad m => CompilerT m CompilerEnv
compilerEnv = ask  

compilerError :: Monad m => CompilerError -> CompilerT m a
compilerError = throwError 

----------------------------------------
-- Compilation environment
----------------------------------------

data CompilerEnv where
  CompilerEnv :: {} -> CompilerEnv
  deriving (Show, Read, Eq, Ord)

----------------------------------------
-- Compilation errors
----------------------------------------

data CompilerError where 
  ParseError     :: {} -> CompilerError
  DesugarError   :: {} -> CompilerError
  TypeCheckError :: {} -> CompilerError
  CodeGenError   :: {} -> CompilerError
  deriving (Show, Read, Eq, Ord)