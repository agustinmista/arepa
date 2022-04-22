module Language.TIM.Interpreter
  ( evalTIM
  , TIMError
  , TIMState
  , module Language.TIM.Interpreter.Types
  ) where

import Control.Monad.Extra
import Control.Monad.State

import Language.TIM.Syntax
import Language.TIM.Prim
import Language.TIM.Interpreter.Types
import Language.TIM.Interpreter.Monad

----------------------------------------
-- TIM interpreter
----------------------------------------

evalTIM :: CodeStore -> IO (Either TIMError [Value], TIMTrace)
evalTIM store = runTIM store $ do
  invokeFunction "main"

----------------------------------------
-- Abstract machine

invokeFunction :: Name -> TIM [Value]
invokeFunction entry = do
  -- Set the program entry point
  setCode [ EnterI (LabelM entry) ]
  -- Prepare the stack with a continuation for main to land to
  pushArgStack (mkClosure [] NullP)
  -- Start running instructions
  loopTIM
  -- When finished, pop the value stack as a result
  getValueStack

loopTIM :: TIM ()
loopTIM = do
  logTIMState
  unlessM (gets finalTIMState) $ do
    stepTIM
    loopTIM

stepTIM :: TIM ()
stepTIM = do
  instr <- fetchInstr
  case instr of
    TakeArgI n -> do
      closures <- takeArgStack n
      ptr <- allocFrame (mkFrame closures)
      setFramePtr ptr
    EnterI mode -> do
      closure <- derefClosure mode
      setCode (closure_code closure)
      setFramePtr (closure_frame closure)
    PushArgI mode -> do
      closure <- derefClosure mode
      pushArgStack closure
    PushValueI mode -> do
      pushValueStack mode
    CallI name -> do
      prim <- lookupPrimOp name
      operateOnValueStack (prim_arity prim) (prim_runner prim)
    ReturnI -> do
      [closure] <- takeArgStack 1
      setCode (closure_code closure)
      setFramePtr (closure_frame closure)
