module Main where

import System.Exit


import CLI
import IO
import Compiler
import Parser
import TypeCheck
import CodeGen

----------------------------------------
-- Entry point
----------------------------------------

main :: IO ()
main = do
  opts <- parseCliOpts
  let env = mkCompilerEnv opts
  (res, msgs) <- runCompiler env compile

  print msgs
  case res of
    Left  ce -> print ce
    Right () -> exitSuccess

-- The compilation pipeline

compile :: Compiler ()
compile = readInput
      >>= parseModule
      >>= typeCheckModule
      >>= emitLLVM
      >>= renderLLVM
      >>= writeOutput
