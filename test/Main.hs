module Main where

import TIMTests (runTimTests)

main :: IO ()
main = do
  putStrLn "Running shitty TIM tests"
  runTimTests
