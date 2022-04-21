module Language.Arepa.Compiler.Codegen
  ( LLVMModule
  , emitLLVM
  , renderLLVM
  ) where

import Control.Monad.State

import Data.Char
import Data.String

import Data.Text.Lazy qualified as Text

import Data.Map (Map)
import Data.Map qualified as Map

import Prettyprinter

import GHC.Exts

import LLVM.AST                        qualified as LLVM
import LLVM.AST.Type                   qualified as LLVM
import LLVM.AST.Typed                  qualified as LLVM
import LLVM.AST.IntegerPredicate       qualified as LLVM
import LLVM.AST.FloatingPointPredicate qualified as LLVM
import LLVM.AST.Constant               qualified as Constant
import LLVM.IRBuilder(MonadModuleBuilder, MonadIRBuilder, ModuleBuilderT, IRBuilderT, buildModuleT, named)
import LLVM.IRBuilder qualified as IR
import LLVM.Pretty

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad
import Language.TIM


----------------------------------------
-- Code generation
----------------------------------------

type LLVMModule = LLVM.Module

emitLLVM :: MonadArepa m => CodeStore -> m LLVMModule
emitLLVM store = do
  let name = fromName (store_name store)
  runLLVM name $ do
    initCodegen store
    emitRTS
    emitCodeStore store

renderLLVM :: MonadArepa m => LLVMModule -> m Text
renderLLVM m = return (ppllvm m)

----------------------------------------
-- Code generation internal state
----------------------------------------

data CodegenState = CodegenState {
  cg_globals :: Map Name LLVM.Operand,   -- Global operands
  cg_context :: [Map Name LLVM.Operand], -- Local operands
  cg_strings :: Map Text LLVM.Operand   -- Global strings
}

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState {
  cg_globals = mempty,
  cg_context = mempty,
  cg_strings = mempty
}

----------------------------------------
-- Code generation monads
----------------------------------------

-- The LLVM module builder monad
type LLVM m a = ModuleBuilderT (StateT CodegenState m) a

-- The constraints needed to write functions
type MonadLLVM m = (
    MonadArepa m,
    MonadState CodegenState m,
    MonadModuleBuilder m
  )

runLLVM :: MonadArepa m => String -> LLVM m a -> m LLVMModule
runLLVM name mb = evalStateT (buildModuleT (fromString name) mb) emptyCodegenState

----------------------------------------
-- Global operands

-- Register the operand associated to a global identifier
-- registerGlobalOperand :: MonadLLVM m => Ident -> LLVM.Operand -> m ()
registerGlobalOperand :: MonadLLVM m => Name -> LLVM.Operand -> m ()
registerGlobalOperand name op = do
  modify' $ \st -> st { cg_globals = Map.insert name op (cg_globals st) }

-- Get the operand associated with an identifier
-- lookupGlobalOperand :: MonadLLVM m => Ident -> m LLVM.Operand
lookupGlobalOperand :: MonadLLVM m => Name -> m LLVM.Operand
lookupGlobalOperand name = do
  globals <- gets cg_globals
  case Map.lookup name globals of
    Nothing -> throwInternalError ("lookupGlobalOperand: operand for global" <+> pretty name <+> "does not exist")
    Just op -> return op

----------------------------------------
-- Local operands

-- Register the operand associated to a variable identifier in the closest context
registerVarOperand :: MonadLLVM m => Name -> LLVM.Operand -> m ()
registerVarOperand name op = do
  ctx <- gets cg_context
  case ctx of
    [] -> throwInternalError "registerVarOperand: null context"
    c:cs -> modify' $ \st -> st { cg_context = Map.insert name op c : cs }

-- Get the operand associated with an identifier
lookupVarOperand :: MonadLLVM m => Name -> m LLVM.Operand
lookupVarOperand name = do
  gets cg_context >>= search
  where
    search []     = throwInternalError ("lookupVarOperand: operand for variable" <+> pretty name <+> "does not exist")
    search (c:cs) = maybe (search cs) return (Map.lookup name c)

-- Run the code inside of a new local context
insideLocalContext :: MonadLLVM m => m a -> m a
insideLocalContext ma = do
  ctx <- gets cg_context
  modify' $ \st -> st { cg_context = Map.empty : ctx }
  a <- ma
  modify' $ \st -> st { cg_context = ctx }
  return a

----------------------------------------
-- Strings literals

-- Register a global string literal
-- If it is already registered, then return the corresponding operand
registerString :: MonadLLVM m => Text -> m LLVM.Operand
registerString str = do
  strings <- gets cg_strings
  case Map.lookup str strings of
    Just op -> return op
    Nothing -> do
      let name = mkGlobalStringName (Map.size strings)
      con <- IR.globalStringPtr (Text.unpack str) name
      let op = LLVM.ConstantOperand con
      modify' $ \st -> st { cg_strings = Map.insert str op strings }
      return op

----------------------------------------
-- Initializing code generator
----------------------------------------

initCodegen :: MonadLLVM m => CodeStore -> m ()
initCodegen store = do
  registerGlobals store

-- Register global declarations before we start emitting code so out-of-order
-- and mutually-recursive functions can be generated using `IR.function`.
--
-- VERY IMPORTANT: this only works because the operand name used by `extern` and
-- `function` is the same as the name of the global, no fresh name is generated.
registerGlobals :: MonadLLVM m => CodeStore -> m ()
registerGlobals store = do
  forM_ (Map.keys (store_blocks store)) $ \name -> do
    let ty = mkVoidFunType
    let op = mkGlobalOperand name ty
    registerGlobalOperand name op

----------------------------------------
-- Emitting RTS code
----------------------------------------

emitRTS :: MonadLLVM m => m ()
emitRTS = return ()

----------------------------------------
-- Emitting user code
----------------------------------------

-- Code stores
emitCodeStore :: MonadLLVM m => CodeStore -> m ()
emitCodeStore store = do
  forM_ (Map.toList (store_blocks store)) $ \(name, code) -> do
    emitCodeBlock name code

-- Code blocks
emitCodeBlock :: MonadLLVM m => Name -> CodeBlock -> m ()
emitCodeBlock name code = void $ do
  IR.function (fromName name) [] LLVM.VoidType $ \_ -> do
    ----------------------------------------
    IR.block `named` (fromName name <> ".entry")
    mapM_ emitInstr (toList code)

-- Instructions
emitInstr :: (MonadIRBuilder m, MonadLLVM m) => Instr -> m ()
emitInstr instr = do
  case instr of
    TakeI n -> do
      notImplemented "emitInstr/TakeI"
    EnterI mode -> do
      am <- emitAddressMode mode
      case mode of
        ArgM {} -> notImplemented "emitInstr/EnterI/ArgM"
        LabelM {} -> notImplemented "emitInstr/EnterI/LabelM"
        LitM {} -> notImplemented "emitInstr/EnterI/LitM"
    PushI mode -> do
      notImplemented "emitInstr/PushI"

-- Addressing modes
emitAddressMode :: MonadLLVM m => AddressMode -> m LLVM.Operand
emitAddressMode mode = do
  case mode of
    ArgM offset -> return (IR.int64 (fromIntegral offset))
    LabelM name -> lookupGlobalOperand name
    LitM lit -> emitLit lit

-- Literals
emitLit :: MonadLLVM m => Lit -> m LLVM.Operand
emitLit (IntL n)      = return (IR.int64 (fromIntegral n))
emitLit (DoubleL n)   = return (IR.double n)
emitLit (CharL c)     = return (IR.int32 (fromIntegral (ord c)))
emitLit (StringL str) = registerString str

----------------------------------------
-- Low-level utilities
----------------------------------------

mkGlobalStringName :: Int -> LLVM.Name
mkGlobalStringName n = LLVM.mkName ("__string__." <> show n)

mkGlobalOperand :: Name -> LLVM.Type -> LLVM.Operand
mkGlobalOperand name ty = LLVM.ConstantOperand (Constant.GlobalReference ty (fromName name))

mkArg :: Name -> (LLVM.Type, IR.ParameterName)
mkArg name = (undefined, fromName name)

mkVoidFunType :: LLVM.Type
mkVoidFunType = LLVM.FunctionType LLVM.VoidType [] False
