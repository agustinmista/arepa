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
      -- Input
      text <- readInput
      ----------------------------------------
      -- Parsing
      psMod <- parseModule text
      whenM (hasDumpEnabled AST) $ do
        dump "Parsed AST" (prettyShow psMod)
      whenM (hasDumpEnabled PPR) $ do
        dump "Pretty-printed AST" (prettyPrint psMod)
      ----------------------------------------
      -- Type checking
      tcMod <- typeCheckModule psMod
      ----------------------------------------
      -- Translation into TIM code
      store <- translateModule tcMod
      whenM (hasDumpEnabled TIM) $ do
        dump "TIM code store" (prettyPrint store)
      ----------------------------------------
      -- Interpretation/compilation
      ifM hasInterpretEnabled
        ----------------------------------------
        -- Interpretation
        (do
            res <- interpretCodeStore store
            whenM hasVerboseEnabled $ do
              dump "Final value stack" (prettyPrint res)
        )
        ----------------------------------------
        -- Compilation
        (do
            ----------------------------------------
            -- Code generation
            llvmMod <- renderLLVM =<< emitLLVM store
            whenM (hasDumpEnabled LLVM) $ do
              dump "Emitted LLVM" llvmMod
            ----------------------------------------
            -- Compilation
            opt    <- lookupCompilerOption optOptimize
            dbg    <- lookupCompilerOption optDebug
            input  <- lookupCompilerOption optInput
            output <- lookupCompilerOption optOutput
            stdout <- liftIO $ compileAndLinkLLVM opt dbg llvmMod input output
            unless (Text.null stdout) $ do
              warning stdout
        )
