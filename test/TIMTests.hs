module TIMTests where

import Language.Arepa.Syntax(CoreDecl,CoreModule,Module(Module),Lit(IntL))
import Language.Arepa.Compiler.Monad (testArepa)
import Language.Arepa.Compiler.Parser (parseDecl)
import Language.TIM.Syntax (CodeStore)
import Language.Arepa.Compiler.Translate (translateModule)
import Language.TIM.Interpreter.Types ( FramePtr(LitP) )
import Language.TIM.Interpreter ( evalTIM )
import Data.Text.Lazy (unpack,Text)
import Language.TIM.Interpreter.Monad (tim_curr_frame)
import Control.Monad.Compiler (prettyPrint)
import qualified Data.Text.Lazy.IO as TIO

k :: IO CoreDecl
k = testArepa . parseDecl $ "(fun k (x y) x)"

s :: IO CoreDecl
s = testArepa . parseDecl $ "(fun s (f g x) (f x (g x)))"

i :: IO CoreDecl
i = testArepa . parseDecl $ "(fun i (x) x)"

addMainToSKIModule :: CoreDecl -> IO CoreModule
addMainToSKIModule main = Module "ski" . (main :) <$> sequence [s,k,i]

ppCodeStore :: CodeStore -> IO ()
ppCodeStore code = TIO.putStrLn (prettyPrint code)

intLiteral :: Int -> FramePtr
intLiteral = LitP . IntL

compileTestFromMain :: Text -> IO CodeStore
compileTestFromMain main = do
  mainCode <- testArepa $ parseDecl main
  fullCode <- addMainToSKIModule mainCode
  testArepa $ translateModule fullCode




ex1, ex2, ex3, ex4 :: IO (Text,FramePtr,CodeStore)
ex1 = (,,) "ex1" (intLiteral 5) <$> compileTestFromMain "(fun main () (i 5))"
ex2 = (,,) "ex2" (intLiteral 5) <$> compileTestFromMain "(fun main () (k 5 0))"
ex3 = (,,) "ex3" (intLiteral 5) <$> compileTestFromMain "(fun main () (s k 0 5))"
ex4 = (,,) "ex4" (intLiteral 5) <$> compileTestFromMain "(fun main () (s k s k 5 0))"

runTimTest :: (Text,FramePtr,CodeStore) -> IO ()
runTimTest (testName,res,code)= do
  timResult <- evalTIM code
  case timResult of
    (Just e, _)       -> error . unpack $ prettyPrint e
    (Nothing, states) ->
      let finalFrame = tim_curr_frame $ last states
          errorMsg n e a = unpack $
                             "ERROR (" <> n <> "):" <>
                             "Result was " <> a <> " " <>
                             "instead of " <> e
      in if finalFrame == res
         then return ()
         else
          error $ errorMsg testName (prettyPrint res) (prettyPrint finalFrame)

runTimTests :: IO ()
runTimTests = do
  tests <- sequence ([ex1,ex2,ex3,ex4] :: [IO (Text,FramePtr,CodeStore)])
  mapM_ runTimTest tests





