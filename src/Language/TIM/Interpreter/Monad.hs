module Language.TIM.Interpreter.Monad where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import Prettyprinter

import Data.Text.Lazy (Text)

import Data.Stack (Stack)
import Data.Stack qualified as Stack

import Data.Heap (Heap, Addr)
import Data.Heap qualified as Heap

import Data.Map qualified as Map

import Language.TIM.Syntax
import Language.TIM.Prim
import Language.TIM.Interpreter.Types
import Data.Maybe (fromJust)
import Control.Monad.Extra (whenM)


----------------------------------------
-- TIM interpreter monad
----------------------------------------

-- TIM monad

newtype TIM a = TIM (ExceptT TIMError (StateT TIMState (WriterT TIMTrace IO)) a)
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadError TIMError
           , MonadState TIMState
           , MonadWriter TIMTrace
           , MonadFail )

runTIM :: CodeStore -> TIM a -> IO (Either TIMError a, TIMTrace)
runTIM code (TIM ma) = runWriterT (evalStateT (runExceptT ma) (initialTIMState code))

-- Machine exceptions

newtype TIMError = TIMError Text
  deriving Show

instance Pretty TIMError where
  pretty (TIMError err) = pretty err

-- Machine traces

newtype TIMTrace = TIMTrace [TIMState]
  deriving Semigroup via [TIMState]
  deriving Monoid    via [TIMState]

instance Pretty TIMTrace where
  pretty (TIMTrace states) = vsep (pretty <$> states)

-- Dump elements
data Dump = Dump {
  dump_frame :: FramePtr,
  dump_stack :: Stack Closure,
  dump_index :: Int
}

-- Machine state

data TIMState = TIMState {
  tim_code_store :: CodeStore,
  tim_curr_codeblock :: CodeBlock,
  tim_curr_frame :: FramePtr,
  tim_heap :: Heap Frame,
  tim_arg_stack :: Stack Closure,
  tim_arg_dump :: Stack Dump,
  tim_value_stack :: Stack Value
}

instance Pretty TIMState where
  pretty st = vsep [
      showDiv,
      showCode,
      showFrame,
      showArgStack,
      showDump,
      showValueStack,
      showDiv
    ]
    where
      showDiv =
        "======================="
      showCode =
        "Current code:" <+> pretty (tim_curr_codeblock st)
      showFrame =
        vsep $
          [ "Current frame pointer:" <+> pretty (tim_curr_frame st) ] <>
          case tim_curr_frame st of
            AddrP addr ->
              let Just frame = Heap.deref addr (tim_heap st) in [ pretty frame ]
            _ -> []
      showArgStack =
        let stack = Stack.toList (tim_arg_stack st) in
        vsep $
          [ "Argument stack:" ] <>
          if null stack
          then [ indent 2 "empty" ]
          else [ indent 2 (pretty closure) | closure <- reverse stack ]
      showDump =
        let dump = Stack.toList (tim_arg_dump st) in
        vsep $
          [ "Argument dump:" ] <>
          if null dump
          then [ indent 2 "empty" ]
          else [ indent 2 "TODO" ]
      showValueStack =
        let stack = Stack.toList (tim_value_stack st) in
        vsep $
          [ "Value stack:" ] <>
          if null stack
          then [ indent 2 "empty" ]
          else [ indent 2 (pretty closure) | closure <- reverse stack ]

initialTIMState :: CodeStore -> TIMState
initialTIMState code = TIMState {
  tim_code_store = code,
  tim_curr_codeblock = mempty,
  tim_curr_frame = NullP,
  tim_heap = Heap.empty,
  tim_arg_stack = Stack.empty,
  tim_arg_dump = Stack.empty,
  tim_value_stack = Stack.empty
}

finalTIMState :: TIMState -> Bool
finalTIMState st = isNullCodeBlock (tim_curr_codeblock st)

----------------------------------------
-- Monad operations

-- Throw an error
throwTIMError :: Text -> TIM a
throwTIMError msg = throwError (TIMError msg)

-- Log the current TIM state
logTIMState :: TIM ()
logTIMState = get >>= tell . TIMTrace . pure

-- Lookup the code of a compiled label
lookupCodeBlock :: Name -> TIM CodeBlock
lookupCodeBlock name = do
  store <- gets tim_code_store
  case lookupCodeStore name store of
    Nothing -> throwTIMError ("lookupCodeBlock: variable " <> fromName name <> " not in the store")
    Just code -> return code

-- Empty the stack
clearStack :: TIM ()
clearStack = modify' $ \st -> st { tim_arg_stack = Stack.empty }

-- Push dump element into the dump
pushArgDump :: Dump -> TIM ()
pushArgDump dump = modify' $ \st ->
  st { tim_arg_dump = Stack.push dump (tim_arg_dump st) }

-- Pushes the current stack into the dump and clears it
pushStackToDump :: Int -> TIM ()
pushStackToDump index = do
  s <- gets tim_arg_stack
  f <- gets tim_curr_frame
  let oldStack = Dump {dump_index = index, dump_frame = f,dump_stack = s }
  pushArgDump oldStack
  clearStack

isCurrentFramePartial :: TIM Bool
isCurrentFramePartial = do
  frame_is_partial <$>  getCurrentFrame

-- Loads all the arguments of a partial frame into the argument stack
populateArgumentStack :: TIM ()
populateArgumentStack = do
  frame <- getCurrentFrame
  unless (frame_is_partial frame)
    (throwTIMError "populateArgumentStack: frame is not partial")
  mapM_ pushArgStack <$> reverse $ frame_closures frame

isArgumentStackBigEnough :: Int -> TIM Bool
isArgumentStackBigEnough n = do
  stack_size <- gets (Stack.size . tim_arg_stack)
  return $ n <= stack_size

popDump :: TIM Dump
popDump = do
  whenM (gets (Stack.isEmpty . tim_arg_dump))
    (throwTIMError "popDump: the stack dump is empty")
  dump <- gets (fromJust . Stack.peek . tim_arg_dump)
  modify' $
    \st -> st { tim_arg_stack =
                  Stack.append (tim_arg_stack st) (dump_stack dump),
                tim_arg_dump  =
                  fromJust $ Stack.pop (tim_arg_dump st) }
  return dump

-- Pop the stack dump and updates the corresponding closure at the offset of the
-- frame saved in the removed dump to have a partial frame with all the arguments
-- currently in the argument stack.
handlePartialApp :: TIM ()
handlePartialApp = do
  Dump framePtr _ index <- popDump
  closures  <- gets (Stack.toList . tim_arg_stack)
  newPartialFrame <- allocFrame $ mkPartialFrame closures
  let updateClosure c = mkClosure (closure_code c) newPartialFrame
  manipulateFramePtr framePtr (manipulateFrame index updateClosure)
  return ()

-- Lookup for a primitive operation
lookupPrimOp :: Name -> TIM Prim
lookupPrimOp name = do
  case Map.lookup name primitives of
    Nothing -> throwTIMError ("lookupPrimOp: primitive " <> fromName name <> " does not exist")
    Just prim -> return prim

-- Fetch the next instruction to execute
fetchInstr :: TIM Instr
fetchInstr = state $ \st ->
  let (next, rest) = splitCodeBlock (tim_curr_codeblock st) in
  (next, st { tim_curr_codeblock = rest })

-- Set the code to execute next
setCode :: CodeBlock -> TIM ()
setCode code = modify' $ \st ->
  st { tim_curr_codeblock = code }

-- Push a closure to the current stack
pushArgStack :: Closure -> TIM ()
pushArgStack closure = modify' $ \st ->
  st { tim_arg_stack = Stack.push closure (tim_arg_stack st) }

-- Take the first `n` elements from the current arg stack
takeArgStack :: Int -> TIM [Closure]
takeArgStack n = do
  st <- get
  case Stack.take n (tim_arg_stack st) of
    Nothing -> throwTIMError "takeArgStack: not enough elements"
    Just (closures, stack) -> do
      put st { tim_arg_stack = stack }
      return closures

-- Push a value to the value stack
pushValueStack :: ValueMode -> TIM ()
pushValueStack mode = do
  st <- get
  case mode of
    FramePtrM -> do
      case tim_curr_frame st of
        ValueP value -> do
          put st { tim_value_stack = Stack.push value (tim_value_stack st) }
        _ -> do
          throwTIMError "pushValueStack: frame pointer does not contain a value"
    InlineM value -> do
      put st { tim_value_stack = Stack.push value (tim_value_stack st) }

-- Transform the value stack (used by primitive operations)
operateOnValueStack :: Prim -> TIM ()
operateOnValueStack prim = do
  st <- get
  case Stack.take (prim_arity prim) (tim_value_stack st) of
    Nothing -> do
      throwTIMError "operateOnValueStack: not enough arguments on the stack"
    Just (args, rest) -> do
      res <- liftIO (prim_runner prim args)
      put st { tim_value_stack = Stack.push res rest }

-- Get the current value stack (only used to return the final values in the
-- interpreter)
getValueStack :: TIM [Value]
getValueStack = do
  st <- get
  return (Stack.toList (tim_value_stack st))

-- Set the current frame pointer
setFramePtr :: FramePtr -> TIM ()
setFramePtr ptr = modify' $ \st ->
  st { tim_curr_frame = ptr }

-- Allocate a frame in the heap
allocFrame :: Frame -> TIM FramePtr
allocFrame frame = state $ \st ->
  let (ptr, heap) = Heap.alloc frame (tim_heap st) in
  (AddrP ptr, st { tim_heap = heap })

-- Dereference a closure in the heap
derefClosure :: ArgMode -> TIM Closure
derefClosure mode = do
  st <- get
  let heap = tim_heap st
  let curr_frame = tim_curr_frame st
  case mode of
    ArgM offset -> do
      case curr_frame of
        AddrP addr -> do
          case Heap.deref addr heap of
            Nothing -> do
              throwTIMError "derefClosure: cannot find frame"
            Just frame -> do
              case frameOffset offset frame of
                Nothing -> do
                  throwTIMError "derefClosure: invalid frame offset"
                Just closure -> do
                  return closure
        _ -> do
          throwTIMError "derefClosure: dereferencing an argument offset requires an address frame pointer"
    LabelM v -> do
      code <- lookupCodeBlock v
      return (mkClosure code curr_frame)
    ValueM lit -> do
      let litCode = [PushValueI FramePtrM, ReturnI]
      return (mkClosure litCode (ValueP lit))

-- Fetches a frame and updates it with the given function.
-- Returns the updated frame or an error if anything goes wrong.
manipulateFrameFromAddr :: Addr -> (Frame -> Maybe Frame) -> TIM Frame
manipulateFrameFromAddr addr f = do
  heap <- gets tim_heap
  case Heap.deref addr heap of
    Nothing -> do
      throwTIMError "manipulateFrame: invalid frame address"
    Just frame -> do
      newFrame <- do
        case f frame of
          Nothing -> throwTIMError "manipulateFrame: update to frame failed (possible wrong offset)"
          Just frm -> return frm
      let newHeap = fromJust $ Heap.update (Just . const newFrame) addr heap
      modify' $ \st -> st { tim_heap = newHeap }
      return newFrame

manipulateFramePtr :: FramePtr -> (Frame -> Maybe Frame) -> TIM Frame
manipulateFramePtr fp f = do
  case fp of
    AddrP addr -> manipulateFrameFromAddr addr f
    _ -> do
      throwTIMError "derefFramePtr: The frame pointer is not to a frame"

derefFramePtr :: FramePtr -> TIM Frame
derefFramePtr fp = do
  manipulateFramePtr fp Just


-- Fetches the current frame and updates it with the given function.
-- Returns the updated current frame or an error if anything goes wrong.
manipulateCurrentFrame :: (Frame -> Maybe Frame) -> TIM Frame
manipulateCurrentFrame f = do
  framePtr <- gets tim_curr_frame
  case framePtr of
    AddrP addr -> manipulateFrameFromAddr addr f
    _ -> do
      throwTIMError "manipulateCurrentFrame: The current frame address is not to a frame pointer"

-- Fetches the current frame
getCurrentFrame :: TIM Frame
getCurrentFrame = do
  manipulateCurrentFrame Just

-- Fetches the current frame
getFrame :: Addr -> TIM Frame
getFrame addr = do
  manipulateFrameFromAddr addr Just

-- Performs pure updates (no errors) to the current frame
updateCurrentFrame :: (Frame -> Frame) -> TIM ()
updateCurrentFrame f = do
  void (manipulateCurrentFrame (Just . f))

-- Update a closure in the current frame
updateFrameSlot :: Offset -> Closure -> TIM ()
updateFrameSlot offset closure = do
  void . manipulateCurrentFrame $ updateFrame offset closure