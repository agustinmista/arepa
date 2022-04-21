module Language.TIM.Interpreter
  ( evalTIM
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

evalTIM :: CodeStore -> IO (Maybe TIMError, [TIMState])
evalTIM store = do
  res <- runTIM store loopTIM
  case res of
    (Left err, trace) -> return (Just err, trace)
    (Right (), trace) -> return (Nothing,  trace)

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
    TakeI n -> takeI n
    EnterI mode -> enterI mode
    PushI mode -> pushI mode

takeI :: Int -> TIM ()
takeI n = do
  closures <- takeStack n
  ptr <- allocFrame (mkFrame closures)
  setFramePtr ptr

enterI :: AddressMode -> TIM ()
enterI mode = do
  closure <- derefClosure mode
  setCode (closure_code closure)
  setFramePtr (closure_frame closure)

pushI :: AddressMode -> TIM ()
pushI mode = do
  closure <- derefClosure mode
  pushStack closure

