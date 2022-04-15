module GMachineTests where

import Parser
import Syntax
import Compiler
import IR.GMachine

-- | This just ignore possible parsing error
--   since the purpose of this module is not to
--   test parsing
right :: Show b => Either b a -> a
right ae = case ae of
      Right a -> a
      Left e  -> error $ show e

-- I x = x
i :: CoreDecl
i = right $ parseDecl "(fun i (x) x)"

-- K x y = x
k :: CoreDecl
k = right $ parseDecl "(fun k (x y) x)"

-- S f g x = f x (g x)
s :: CoreDecl
s = right $ parseDecl "(fun s (f g x) (f x (g x)))"

addMainToSKIModule :: CoreDecl -> CoreModule
addMainToSKIModule main = Module "ski" (main : [s,k,i])

ex1, ex2, ex3, ex4 :: (CoreDecl ,GmNode)
ex1 = (right $ parseDecl "(fun main () (i 5))", NNum 5)
ex2 = (right $ parseDecl "(fun main () (k 5 0))", NNum 5)
ex3 = (right $ parseDecl "(fun main () (s k 0 5))", NNum 5)
ex4 = (right $ parseDecl "(fun main () (s k s k 5 0))", NNum 5)

testGm :: CoreDecl -> GmNode -> Bool
testGm main result = evalGm (compiler (addMainToSKIModule main)) == result

test1,test2,test3,test4 :: Bool
test1 = let (c,r) = ex1 in testGm c r
test2 = let (c,r) = ex2 in testGm c r
test3 = let (c,r) = ex3 in testGm c r
test4 = let (c,r) = ex4 in testGm c r
