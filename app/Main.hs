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
  store <- translate llMod
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

lambdaLift :: MonadArepa m => CoreModule -> m CoreModule
lambdaLift tcMod = do
  llMod <- lambdaLiftModule tcMod
  whenDump LIFT $ dump "Lambda lifted module" (prettyPrint llMod)
  return llMod

translate :: MonadArepa m => CoreModule -> m CodeStore
translate tcMod = do
  store <- translateModule tcMod
  whenDump TIM $ dump "TIM code store" (prettyPrint store)
  tim_path <- compiledTIMPath
  let tim_text = encodeCodeStore store
  writeCompiledFile tim_path tim_text
  return store

interpret :: MonadArepa m => CodeStore -> m ()
interpret store = do
  interpretCodeStore store

codegen :: MonadArepa m => CodeStore -> m ()
codegen store = do
  cmod <- emitC store
  (h_text, c_text) <- renderC cmod
  whenDump CG $ dump "Emitted C header" h_text
  h_path <- compiledHPath
  writeCompiledFile h_path h_text
  whenDump CG $ dump "Emitted C code"   c_text
  c_path <- compiledCPath
  writeCompiledFile c_path c_text

link :: MonadArepa m => m ()
link = unlessM hasLinkingDisabled $ do
  args <- mkClangArgs
  runClang args