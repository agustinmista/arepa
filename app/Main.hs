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
      ----------------------------------------
      text <- readInput
      ----------------------------------------
      psMod <- parseModule text
      whenM (hasDumpEnabled AST) $ do
        debugMsg "Parsed AST" (Just (prettyShow psMod))
      whenM (hasDumpEnabled PPR) $ do
        debugMsg "Pretty-printed AST" (Just (prettyPrint psMod))
      ----------------------------------------
      tcMod <- typeCheckModule psMod
      ----------------------------------------
      store <- translateModule tcMod
      whenM (hasDumpEnabled TIM) $ do
        debugMsg "TIM code store" (Just (prettyPrint store))
      ----------------------------------------
      llvmMod <- renderLLVM =<< emitLLVM store
      whenM (hasDumpEnabled PPR) $ do
        debugMsg "Emitted LLVM" (Just llvmMod)
      ----------------------------------------
      writeOutput llvmMod