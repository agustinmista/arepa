module Language.Arepa.Compiler.Monad
  ( module Language.Arepa.Compiler.Monad
  , module Control.Monad.Compiler
  , Text
  ) where

import Data.Void

import Text.Megaparsec

import Data.Text.Lazy (Text)

import Prettyprinter

import Control.Monad.Compiler


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
-- Compilation errors

data ArepaError =
    ParserError ParserError
  | CodegenError CodegenError
  deriving Show

instance Pretty ArepaError where
  pretty (ParserError err) = viaShow (errorBundlePretty err)
  pretty (CodegenError err) = viaShow err

-- Parse errors
type ParserError = ParseErrorBundle Text Void

throwParserError :: MonadArepa m => ParserError -> m a
throwParserError err = throwCompilerError (ParserError err)

-- Other errors (to be completed)
type CodegenError = Doc ()

throwCodegenError :: MonadArepa m => CodegenError -> m a
throwCodegenError err = throwCompilerError (CodegenError err)


----------------------------------------
-- Compiler options

data ArepaOpts = ArepaOpts {
  optInput :: Maybe FilePath,    -- Nothing means stdin
  optOutput :: Maybe FilePath,   -- Nothing means stdout
  optDump :: [DumpOpt],
  optVerbose :: Bool
} deriving (Show, Read, Eq, Ord)

defaultOpts :: ArepaOpts
defaultOpts :: ArepaOpts = ArepaOpts {
  optInput = Nothing,
  optOutput = Nothing,
  optDump = [],
  optVerbose = False
}

-- Dump options
data DumpOpt = AST | PPR | TIM | LLVM
  deriving (Show, Read, Eq, Ord)

hasDumpEnabled :: MonadArepa m => DumpOpt -> m Bool
hasDumpEnabled opt = (opt `elem`) <$> lookupCompilerOption optDump