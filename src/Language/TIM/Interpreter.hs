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
runCodeStore :: FilePath -> FilePath -> CodeStore -> IO [Value]
runCodeStore stdin stdout store = do
  (res, _) <- runTIM stdin stdout store $ do
    invokeFunction "main" []
  case res of
    Left err     -> error (show err)
    Right values -> return values

-- Invoke a function with some arguments in the current code store
invokeFunction :: Name -> [Value] -> TIM [Value]
invokeFunction fun args = do
  -- Prepare the stack with a continuation for main to land to
  pushArgumentStack (mkClosure [] NullP)
  -- Set the program code, which pushes (inline) the input arguments to the
  -- argument stack and proceeds to jumps to the entry point
  setCode $ foldMap (\arg -> [ PushArgI (ValueM arg) ] ) args <> [ EnterI (LabelM fun) ]
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
    instr <- fetchInstr
    stepTIM instr
    loopTIM

stepTIM :: Instr -> TIM ()
stepTIM instr = do
  case instr of
    TakeArgI t n -> do
      stackClosures <- takeArgumentStack n
      let localClosures = replicate (t - n) dummyClosure
      ptr <- allocFrame (mkFrame (stackClosures <> localClosures))
      setCurrentFramePtr ptr
      nextInstr
    PushArgI mode -> do
      closure <- derefClosure mode
      pushArgumentStack closure
      nextInstr
    PushValueI mode -> do
      pushValueStack mode
      nextInstr
    PushMarkerI index -> do
      pushArgumentStackToDump index
      nextInstr
    UpdateMarkersI n -> do
      unless (n == 0) $ do
        whenM isCurrentFramePartial $ do
          populateArgumentStackFromPartialFrame
        unlessM (isArgumentStackBigEnough n) $ do
          handlePartialApp
      nextInstr
    EnterI mode -> do
      closure <- derefClosure mode
      setCurrentFramePtr (closure_frame closure)
      setCode (closure_code closure)
    MoveI n mode -> do
      closure <- derefClosure mode
      updateCurrentFrameSlot n closure
      nextInstr
    ReturnI -> do
      ifM isArgumentStackEmpty
        returnWithEmptyArgumentStack
        returnToContinuation
    CallI name -> do
      prim <- lookupPrim name
      operateOnValueStack prim
      nextInstr

----------------------------------------
-- TIM Operations

-- The default return behavior: look for a saved continuation in the argument
-- stack and jump to it.
returnToContinuation :: TIM ()
returnToContinuation = do
  closure <- popArgumentStack
  setCurrentFramePtr (closure_frame closure)
  setCode (closure_code closure)

-- The return behavior used by sharing / updates: when the argument stack is
-- empty, pop the previous stack dump into it and update the corresponding
-- closure at the offset of the frame saved in the removed dump to a value
-- closure corresponding to the top of the value stack.
returnWithEmptyArgumentStack :: TIM ()
returnWithEmptyArgumentStack = void $ do
  (framePtr, index) <- popDumpIntoArgumentStack
  value <- peekValueStack
  closure <- derefClosure (ValueM value)
  manipulateFramePtr framePtr (updateFrame index closure)

-- Loads all the arguments of a partial frame into the argument stack.
populateArgumentStackFromPartialFrame :: TIM ()
populateArgumentStackFromPartialFrame = do
  unlessM isCurrentFramePartial $ do
    throwTIMError "populateArgumentStackFromPartialFrame: frame is not partial"
  closures <- frame_closures <$> getCurrentFrame
  mapM_ pushArgumentStack (reverse closures)

-- Pop the stack dump and updates the corresponding closure at the offset of the
-- frame saved in the removed dump to have a partial frame with all the
-- arguments currently in the argument stack.
handlePartialApp :: TIM ()
handlePartialApp = void $ do
  (framePtr, index) <- popDumpIntoArgumentStack
  closures  <- getArgumentStack
  newPartialFrame <- allocFrame (mkPartialFrame closures)
  let updateClosure c = mkClosure (closure_code c) newPartialFrame
  manipulateFramePtr framePtr (manipulateFrame index updateClosure)