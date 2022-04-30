module Main where

import Control.Monad.Extra

import Data.Text.Lazy qualified as Text

import Language.Arepa.Compiler

import CLI
import Clang

----------------------------------------
-- Entry point
----------------------------------------

main :: IO ()
main = do
  opts <- parseCliOpts
  runArepa' opts compiler

compiler :: MonadArepa m => m ()
compiler = do
    handleCompilerError printCompilerError $ do
      ----------------------------------------
      text <- readInput
      ----------------------------------------
      psMod <- parseModule text
      whenM (hasDumpEnabled AST) $ do
        dump "Parsed AST" (prettyShow psMod)
      whenM (hasDumpEnabled PPR) $ do
        dump "Pretty-printed AST" (prettyPrint psMod)
      ----------------------------------------
      tcMod <- typeCheckModule psMod
      ----------------------------------------
      store <- translateModule tcMod
      whenM (hasDumpEnabled TIM) $ do
        dump "TIM code store" (prettyPrint store)
      ----------------------------------------
      ifM hasInterpretEnabled
        ----------------------------------------
        (do res <- interpretCodeStore store
            whenM hasVerboseEnabled $ do
              dump "Final value stack" (prettyPrint res)
        )
        ----------------------------------------
        (do llvmMod <- renderLLVM =<< emitLLVM store
            whenM (hasDumpEnabled LLVM) $ do
              dump "Emitted LLVM" llvmMod

            opt    <- lookupCompilerOption optOptimize
            input  <- lookupCompilerOption optInput
            output <- lookupCompilerOption optOutput

            stdout <- liftIO $ compileAndLinkLLVM opt llvmMod input output

            unless (Text.null stdout) $ do
              warning stdout
        )
      ----------------------------------------
