module Language.Arepa.Compiler.Codegen
  ( LLVMModule
  , emitLLVM
  , renderLLVM
  ) where

import Control.Monad.State

import Data.Char
import Data.String
import Data.Map (Map)
import Data.Map qualified as Map

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

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

----------------------------------------
-- Code generation
----------------------------------------

type LLVMModule = LLVM.Module

emitLLVM :: CoreModule -> Compiler LLVMModule
emitLLVM m = do
  let name = fromVar (mod_name m)
  runLLVM name $ do
    initCodegen m
    emitRTS 
    emitModule m

renderLLVM :: LLVMModule -> Compiler Text
renderLLVM m = return (ppllvm m)

----------------------------------------
-- Code generation internal state
----------------------------------------

data CodegenState = CodegenState {
  cg_globals :: Map Var LLVM.Operand,   -- Global operands
  cg_context :: [Map Var LLVM.Operand], -- Local operands
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
type LLVM = ModuleBuilderT (StateT CodegenState (CompilerT IO))

-- The constraints needed to write functions
type MonadLLVM m = (
    MonadError CompilerError m,
    MonadState CodegenState m,
    MonadModuleBuilder m
  )

runLLVM :: String -> LLVM a -> Compiler LLVMModule
runLLVM name mb = evalStateT (buildModuleT (fromString name) mb) emptyCodegenState

----------------------------------------
-- Global operands

-- Register the operand associated to a global identifier
-- registerGlobalOperand :: MonadLLVM m => Ident -> LLVM.Operand -> m ()
registerGlobalOperand :: MonadLLVM m => Var -> LLVM.Operand -> m ()
registerGlobalOperand name op = do
  modify' $ \st -> st { cg_globals = Map.insert name op (cg_globals st) }

-- Get the operand associated with an identifier
-- lookupGlobalOperand :: MonadLLVM m => Ident -> m LLVM.Operand
lookupGlobalOperand :: MonadLLVM m => Var -> m LLVM.Operand
lookupGlobalOperand name = do
  globals <- gets cg_globals
  case Map.lookup name globals of
    Nothing -> throwCodegenError ("operand for global " <> ppr name <> " does not exist")
    Just op -> return op

----------------------------------------
-- Local operands

-- Register the operand associated to a variable identifier in the closest context
registerVarOperand :: MonadLLVM m => Var -> LLVM.Operand -> m ()
registerVarOperand name op = do
  ctx <- gets cg_context
  case ctx of
    [] -> throwCodegenError "empty context in registerVarOperand"
    c:cs -> modify' $ \st -> st { cg_context = Map.insert name op c : cs }

-- Get the operand associated with an identifier
lookupVarOperand :: MonadLLVM m => Var -> m LLVM.Operand
lookupVarOperand name = do
  gets cg_context >>= search
  where
    search []     = throwCodegenError ("operand for variable " <> ppr name <> " does not exist")
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

initCodegen :: CoreModule -> LLVM ()
initCodegen m = do
  registerGlobals m

-- Register global declarations before we start emitting code so out-of-order
-- and mutually-recursive functions can be generated using `IR.function`.
--
-- VERY IMPORTANT: this only works because the operand name used by `extern` and
-- `function` is the same as the name of the global, no fresh name is generated.
registerGlobals :: CoreModule -> LLVM ()
registerGlobals m = do
  forM_ (mod_decls m) $ \decl -> do
    case decl of
      ValD name _ -> do
        let ty = undefined
        let op = mkGlobalOperand name ty 
        registerGlobalOperand name op
      FunD name _ _ -> do
        let ty = undefined
        let op = mkGlobalOperand name ty
        registerGlobalOperand name op

----------------------------------------
-- Emitting RTS code
----------------------------------------

emitRTS :: LLVM ()
emitRTS = return ()

----------------------------------------
-- Emitting user code
----------------------------------------

-- Modules

emitModule :: CoreModule -> LLVM ()
emitModule m = do
  mapM_ emitDecl (mod_decls m)

-- Declarations

emitDecl :: CoreDecl -> LLVM ()
emitDecl (ValD name body) = do
  undefined
emitDecl (FunD name args body) = void $ do
  IR.function (fromVar name) (mkArg <$> args) undefined $ \ops -> do
    ----------------------------------------
    IR.block `named` (fromVar name <> ".entry")
    -- Allocate and register the function arguments
    forM_ (zip args ops) $ \(argname, argop) -> do
      addr <- IR.alloca (LLVM.typeOf argop) Nothing 0 `named` fromVar argname
      registerVarOperand argname addr
      IR.store addr 0 argop
    -- Do something for the body of the function
    undefined

-- Expressions

emitExpr :: CoreExpr -> LLVM LLVM.Operand
emitExpr = do
  undefined

-- Case alternatives

emitAlt :: CoreAlt -> LLVM LLVM.Operand
emitAlt = do
  undefined

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

mkGlobalOperand :: Var -> LLVM.Type -> LLVM.Operand
mkGlobalOperand name ty = LLVM.ConstantOperand (Constant.GlobalReference ty (fromVar name))

mkArg :: Var -> (LLVM.Type, IR.ParameterName)
mkArg name = (undefined, fromVar name) 
