module CodeGen 
  ( LLVMModule
  , emitLLVM
  , renderLLVM
  ) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

import LLVM.AST                        qualified as LLVM
import LLVM.AST.Type                   qualified as LLVM
import LLVM.AST.Typed                  qualified as LLVM
import LLVM.AST.IntegerPredicate       qualified as LLVM
import LLVM.AST.FloatingPointPredicate qualified as LLVM
import LLVM.AST.Constant               qualified as Constant

import LLVM.IRBuilder (MonadModuleBuilder, MonadIRBuilder, ModuleBuilderT, IRBuilderT, buildModuleT, named)
import LLVM.IRBuilder qualified as IR

import LLVM.Pretty

import Data.String

import Syntax
import Compiler

----------------------------------------
-- Code generation
----------------------------------------

type LLVMModule = LLVM.Module

emitLLVM :: CoreModule -> Compiler LLVMModule
emitLLVM m = do
  let name = fromString (varString (mod_name m))
  IR.buildModuleT name $ do
    return () 

renderLLVM :: LLVMModule -> Compiler Text
renderLLVM m = return (ppllvm m) 