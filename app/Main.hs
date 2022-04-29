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
  runArepa' opts compiler

compiler :: MonadArepa m => m ()
compiler = do
    handleCompilerError printCompilerError $ do
      ----------------------------------------
      text <- readInput
      ----------------------------------------
      psMod <- parseModule text
      whenM (hasDumpEnabled AST) $ debugMsg "Parsed AST" (Just (prettyShow psMod))
      whenM (hasDumpEnabled PPR) $ debugMsg "Pretty-printed AST" (Just (prettyPrint psMod))
      ----------------------------------------
      tcMod <- typeCheckModule psMod
      ----------------------------------------
      store <- translateModule tcMod
      whenM (hasDumpEnabled TIM) $ debugMsg "TIM code store" (Just (prettyPrint store))
      ----------------------------------------
      ifM hasInterpretEnabled
        ----------------------------------------
        (do res <- interpretCodeStore store
            whenM hasVerboseEnabled $ debugMsg "Final value stack" (Just (prettyPrint res)))
        ----------------------------------------
        (do llvmMod <- renderLLVM =<< emitLLVM store
            whenM (hasDumpEnabled PPR) $ debugMsg "Emitted LLVM" (Just llvmMod))
      ----------------------------------------
