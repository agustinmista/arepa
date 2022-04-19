module Main where

import Control.Monad.Extra

import Language.Arepa.Compiler
import CLI

----------------------------------------
-- Entry point
----------------------------------------

main :: IO ()
main = do
  opts <- parseCliOpts
  runArepa' opts $
    handleCompilerError printCompilerError $ do
      text <- readInput
      psMod <- parseModule text
      whenM (hasDumpEnabled AST) $ do
        debugMsg "Parsed AST" (Just (show psMod))
      whenM (hasDumpEnabled PPR) $ do
        debugMsg "Pretty-printed AST" (Just psMod)
      tcMod <- typeCheckModule psMod
      llvmMod <- renderLLVM =<< emitLLVM tcMod
      writeOutput llvmMod