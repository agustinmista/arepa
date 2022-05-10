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
  text  <- readArepaInput
  psMod <- parse text
  tcMod <- typecheck psMod
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

typecheck :: MonadArepa m => CoreModule -> m CoreModule
typecheck psMod = do
  typeCheckModule psMod

translate :: MonadArepa m => CoreModule -> m CodeStore
translate tcMod = do
  store <- translateModule tcMod
  whenDump TIM $ dump "TIM code store" (prettyPrint store)
  return store

interpret :: MonadArepa m => CodeStore -> m ()
interpret store = do
  interpretCodeStore store

codegen :: MonadArepa m => CodeStore -> m ()
codegen store = do
  llvm <- emitLLVM store
  text <- renderLLVM llvm
  whenDump LLVM $ dump "Emitted LLVM" text
  writeLLVMOutput text

link :: MonadArepa m => m ()
link = unlessM hasLinkingDisabled $ do
  args <- mkClangArgs
  whenVerbose $ dump "Calling clang with CLI argumnents" (prettyPrint args)
  runClang args