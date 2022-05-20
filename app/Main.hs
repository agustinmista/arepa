module Main where

import Control.Monad.Extra

import Data.Text.Lazy

import Language.Arepa

import CLI

----------------------------------------
-- Entry point
----------------------------------------

main :: IO ()
main = do
  opts <- parseCliOpts
  runArepa' opts compiler

compiler :: MonadArepa m => m ()
compiler = handleCompilerError printCompilerError $ do
  text  <- readArepaSourceFile
  psMod <- parse text
  lnMod <- lint psMod
  rnMod <- rename lnMod
  llMod <- lambdaLift rnMod
  tcMod <- typecheck llMod
  store <- translate tcMod
  ifM hasInterpretEnabled
    (interpret store)
    (codegen store >> link)

parse :: MonadArepa m => Text -> m CoreModule
parse text = do
  psMod <- parseModule text
  whenDump AST $ dump "Parsed AST" (prettyShow psMod)
  whenDump PPR $ dump "Pretty-printed AST" (prettyPrint psMod)
  return psMod

lint :: MonadArepa m => CoreModule -> m CoreModule
lint psMod = do
  lnMod <- lintModule psMod
  whenDump LINT $ dump "Linted module" (prettyPrint lnMod)
  return lnMod

rename :: MonadArepa m => CoreModule -> m CoreModule
rename psMod = do
  rnMod <- renameModule psMod
  whenDump RENAME $ dump "Renamed module" (prettyPrint rnMod)
  return rnMod

typecheck :: MonadArepa m => CoreModule -> m CoreModule
typecheck psMod = do
  typeCheckModule psMod

lambdaLift :: MonadArepa m => CoreModule -> m CoreModule
lambdaLift tcMod = do
  llMod <- lambdaLiftModule tcMod
  whenDump LIFT $ dump "Lambda lifted module" (prettyPrint llMod)
  return llMod

translate :: MonadArepa m => CoreModule -> m CodeStore
translate tcMod = do
  store <- translateModule tcMod
  whenDump TIM $ dump "TIM code store" (prettyPrint store)
  writeTIMCodeStore store
  return store

interpret :: MonadArepa m => CodeStore -> m ()
interpret store = do
  interpretCodeStore store

codegen :: MonadArepa m => CodeStore -> m ()
codegen store = do
  llvm <- emitLLVM store
  text <- renderLLVM llvm
  whenDump LLVM $ dump "Emitted LLVM" text
  writeLLVMModule text

link :: MonadArepa m => m ()
link = unlessM hasLinkingDisabled $ do
  args <- mkClangArgs
  runClang args