module Main where

import System.IO.Temp
import System.FilePath

import Control.Monad.Extra

import Data.Maybe

import Data.Text.Lazy    qualified as Text
import Data.Text.Lazy.IO qualified as Text

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
      whenDump AST $ dump "Parsed AST" (prettyShow psMod)
      whenDump PPR $ dump "Pretty-printed AST" (prettyPrint psMod)
      ----------------------------------------
      -- Type checking
      tcMod <- typeCheckModule psMod
      ----------------------------------------
      -- Translation into TIM code
      store <- translateModule tcMod
      whenDump TIM $ dump "TIM code store" (prettyPrint store)
      ----------------------------------------
      -- Interpretation/compilation
      ifM hasInterpretEnabled
        ----------------------------------------
        -- Interpretation
        (do
            res <- interpretCodeStore store
            whenVerbose $ dump "Final value stack" (prettyPrint res)
        )
        ----------------------------------------
        -- Compilation
        (do
            ----------------------------------------
            -- Code generation
            llvm <- renderLLVM =<< emitLLVM store
            whenDump LLVM $ dump "Emitted LLVM" llvm
            ----------------------------------------
            -- Saving generated LLVM
            input <- lookupCompilerOption optInput
            llpath <- case input of
              Nothing -> do
                liftIO $ emptyTempFile "." "arepa.ll"
              Just path -> do
                return (replaceExtension path "ll")
            liftIO $ Text.writeFile llpath llvm
            ----------------------------------------
            -- Linking (optional if -o/--output)
            output <- lookupCompilerOption optOutput
            when (isJust output) $ do
              opt   <- lookupCompilerOption optOptimize
              dbg   <- lookupCompilerOption optDebug
              extra <- lookupCompilerOption optInclude
              stdout <- liftIO $ runClang opt dbg extra llpath (fromJust output)
              unless (Text.null stdout) $ do
                warning stdout
        )
