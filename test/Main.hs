module Main where

import Test.Tasty

import Good

main :: IO ()
main = do
  goodTests <- loadGoodTests
  defaultMain $
    testGroup "all tests" [
      goodTests
    ]
