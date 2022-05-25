module TIMTests where

import Language.Arepa
import Language.TIM

import Data.Text.Lazy (unpack, Text)
import Data.Text.Lazy.IO qualified as TIO

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

compileTestFromMain :: Text -> IO CodeStore
compileTestFromMain main = do
  mainCode <- testArepa $ parseDecl main
  fullCode <- addMainToSKIModule mainCode
  testArepa $ translateModule fullCode




ex1, ex2, ex3, ex4 :: IO (Text,Value,CodeStore)
ex1 = (,,) "ex1" (IntV 5) <$> compileTestFromMain "(fun main () (i 5))"
ex2 = (,,) "ex2" (IntV 5) <$> compileTestFromMain "(fun main () (k 5 0))"
ex3 = (,,) "ex3" (IntV 5) <$> compileTestFromMain "(fun main () (s k 0 5))"
ex4 = (,,) "ex4" (IntV 5) <$> compileTestFromMain "(fun main () (s k s k 5 0))"

runTimTest :: (Text,Value,CodeStore) -> IO ()
runTimTest (testName,res,code)= do
  valueStack <- runCodeStore False "input.txt" "output.txt" [code]
  let finalValue = head valueStack
      errorMsg n e a = unpack $
                             "ERROR (" <> n <> "):" <>
                             "Result was " <> a <> " " <>
                             "instead of " <> e
      in if finalValue == res
         then return ()
         else
          error $ errorMsg testName (prettyPrint res) (prettyPrint finalValue)

runTimTests :: IO ()
runTimTests = do
  tests <- sequence ([ex1,ex2,ex3,ex4] :: [IO (Text,Value,CodeStore)])
  mapM_ runTimTest tests





