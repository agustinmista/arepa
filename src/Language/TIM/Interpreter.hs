module Language.TIM.Interpreter
  ( runTIM
  , runCodeStore
  , invokeFunction
  , TIMError
  , TIMState
  , module Language.TIM.Interpreter.Types
  ) where

import Control.Monad.Extra
import Control.Monad.State

import Language.TIM.Syntax
import Language.TIM.Interpreter.Types
import Language.TIM.Interpreter.Monad

----------------------------------------
-- TIM interpreter
----------------------------------------

-- Like `runTIM` but loads the code store and invokes main.
-- For debugging purposes mostly.
runCodeStore :: CodeStore -> IO [Value]
runCodeStore store = do
  (res, _) <- runTIM store $ invokeFunction "main" []
  case res of
    Left err     -> error (show err)
    Right values -> return values

-- Invoke a function with some arguments in the current code store
invokeFunction :: Name -> [Value] -> TIM [Value]
invokeFunction fun args = do
  -- Set the program code, which pushes (inline) the input arguments to the
  -- argument stack and proceeds to jumps to the entry point
  setCode $ foldMap (\arg -> [ PushArgI (ValueM arg) ] ) args <> [ EnterI (LabelM fun) ]
  -- Prepare the stack with a continuation for main to land to
  pushArgStack (mkClosure [] NullP)
  -- Start running instructions
  loopTIM
  -- When finished, pop the value stack as a result
  getValueStack

----------------------------------------
-- Internals

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
    TakeArgI t n -> do
      stackClosures <- takeArgStack n
      let localClosures = replicate (t - n) dummyClosure
      ptr <- allocFrame (mkFrame (stackClosures <> localClosures))
      setFramePtr ptr
    PushArgI mode -> do
      closure <- derefClosure mode
      pushArgStack closure
    PushValueI mode -> do
      pushValueStack mode
    EnterI mode -> do
      closure <- derefClosure mode
      setCode (closure_code closure)
      setFramePtr (closure_frame closure)
    MoveI n mode -> do
      closure <- derefClosure mode
      updateFrameSlot n closure
    ReturnI -> do
      [closure] <- takeArgStack 1
      setCode (closure_code closure)
      setFramePtr (closure_frame closure)
    CallI name -> do
      prim <- lookupPrimOp name
      operateOnValueStack prim
