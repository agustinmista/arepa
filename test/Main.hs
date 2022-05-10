module Main where

import Test.Tasty
import Golden

main :: IO ()
main = do
  goldenTests <- loadGoldenTests
  defaultMain $ testGroup "all tests" [
      goldenTests
    ]
