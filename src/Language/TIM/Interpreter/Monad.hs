module Language.TIM.Interpreter.Monad
  ( TIM
  , TIMError
  , TIMState
  , allocFrame
  , derefClosure
  , fetchInstr
  , finalTIMState
  , getArgumentStack
  , getCurrentFrame
  , getValueStack
  , isArgumentStackBigEnough
  , isArgumentStackEmpty
  , isCurrentFramePartial
  , logTIMState
  , lookupPrim
  , manipulateFramePtr
  , nextInstr
  , operateOnValueStack
  , peekValueStack
  , popArgumentStack
  , popDumpIntoArgumentStack
  , pushArgumentStack
  , pushArgumentStackToDump
  , pushValueStack
  , runTIM
  , setCode
  , setCurrentFramePtr
  , takeArgumentStack
  , throwTIMError
  , updateCurrentFrameSlot
  ) where

import Control.Monad.Extra
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import Prettyprinter

import Data.Maybe

import Data.Text.Lazy (Text)

import Data.Stack (Stack)
import Data.Stack qualified as Stack

import Data.Heap (Heap, Addr)
import Data.Heap qualified as Heap

import Data.Map qualified as Map

import Language.TIM.Syntax
import Language.TIM.Prim
import Language.TIM.Interpreter.Types
import Language.TIM.Interpreter.Foreign

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

runTIM :: FilePath -> FilePath -> CodeStore -> TIM a -> IO (Either TIMError a, TIMTrace)
runTIM stdin stdout code (TIM ma) = withRTSIO stdin stdout $ do
  runWriterT (evalStateT (runExceptT ma) (initialTIMState code))

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
  dump_stack :: Stack Closure,
  dump_frame :: FramePtr,
  dump_index :: Int
}

-- Machine state

data TIMState = TIMState {
  tim_code_store :: CodeStore,
  tim_curr_codeblock :: CodeBlock,
  tim_curr_frame :: FramePtr,
  tim_heap :: Heap Frame,
  tim_arg_stack :: Stack Closure,
  tim_dump :: Stack Dump,
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
        let dump = Stack.toList (tim_dump st) in
        vsep $
          [ "Argument dump:" ] <>
          if null dump
          then [ indent 2 "empty" ]
          else [ indent 2 $ vsep $
                  [ "stack:" <+>
                    brackets ("frame ptr=" <> pretty ptr) <+>
                    brackets ("frame index=" <> pretty idx) ] <>
                  [ indent 2 (pretty closure)
                  | closure <- reverse (Stack.toList stack) ]
               | Dump stack ptr idx <- reverse dump ]
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
  tim_dump = Stack.empty,
  tim_value_stack = Stack.empty
}

finalTIMState :: TIMState -> Bool
finalTIMState st = isNullCodeBlock (tim_curr_codeblock st)

----------------------------------------
-- Monad operations
----------------------------------------

----------------------------------------
-- Code blocks

-- Lookup the code of a compiled label
lookupCodeBlock :: Name -> TIM CodeBlock
lookupCodeBlock name = do
  store <- gets tim_code_store
  case lookupCodeStore name store of
    Nothing -> throwTIMError ("lookupCodeBlock: variable " <> fromName name <> " not in the store")
    Just code -> return code

-- Set the code to execute next
setCode :: CodeBlock -> TIM ()
setCode code = modify' $ \st ->
  st { tim_curr_codeblock = code }

-- Fetch the next instruction to execute
fetchInstr :: TIM Instr
fetchInstr = do
  code <- gets tim_curr_codeblock
  return (fst (splitCodeBlock code))

-- Advance to the next instruction
nextInstr :: TIM ()
nextInstr = modify' $ \st ->
  st { tim_curr_codeblock = snd (splitCodeBlock (tim_curr_codeblock st)) }

----------------------------------------
-- Argument stack

isArgumentStackEmpty :: TIM Bool
isArgumentStackEmpty = do
  Stack.isEmpty <$> gets tim_arg_stack

isArgumentStackBigEnough :: Int -> TIM Bool
isArgumentStackBigEnough n = do
  stack_size <- Stack.size <$> gets tim_arg_stack
  return (n <= stack_size)

getArgumentStack :: TIM [Closure]
getArgumentStack = do
  Stack.toList <$> gets tim_arg_stack

-- Empty the argument stack
clearArgumentStack :: TIM ()
clearArgumentStack = modify' $ \st ->
  st { tim_arg_stack = Stack.empty }

pushArgumentStack :: Closure -> TIM ()
pushArgumentStack closure = modify' $ \st ->
  st { tim_arg_stack = Stack.push closure (tim_arg_stack st) }

popArgumentStack :: TIM Closure
popArgumentStack = do
  st <- get
  case Stack.pop (tim_arg_stack st) of
    Nothing -> do
      throwTIMError "popArgumentStack: empty argument stack"
    Just (closure, stack) -> do
      put st { tim_arg_stack = stack }
      return closure

-- Take the first `n` elements from the current arg stack
takeArgumentStack :: Int -> TIM [Closure]
takeArgumentStack n = do
  st <- get
  case Stack.take n (tim_arg_stack st) of
    Nothing -> do
      throwTIMError "takeArgumentStack: not enough elements"
    Just (closures, stack) -> do
      put st { tim_arg_stack = stack }
      return closures

-- Pushes the current stack into the dump and clears it
pushArgumentStackToDump :: Int -> TIM ()
pushArgumentStackToDump index = do
  s <- gets tim_arg_stack
  f <- gets tim_curr_frame
  let oldStack = Dump { dump_index = index, dump_frame = f, dump_stack = s }
  pushDump oldStack
  clearArgumentStack

----------------------------------------
-- Argument dump

pushDump :: Dump -> TIM ()
pushDump dump = modify' $ \st ->
  st { tim_dump = Stack.push dump (tim_dump st) }

-- Pops and prepends the previous dump into the argument stack.
-- Returns the saved frame pointer and closure index
popDumpIntoArgumentStack :: TIM (FramePtr, Int)
popDumpIntoArgumentStack = do
  stack_dump <- gets tim_dump
  case Stack.pop stack_dump of
    Nothing -> do
      throwTIMError "popDumpIntoArgumentStack: empty dump"
    Just (dump, stack_dump') -> do
      modify' $ \st -> st {
        tim_arg_stack = Stack.append (tim_arg_stack st) (dump_stack dump),
        tim_dump = stack_dump'
      }
      return (dump_frame dump, dump_index dump)

----------------------------------------
-- Value stack

getValueStack :: TIM [Value]
getValueStack = do
  Stack.toList <$> gets tim_value_stack

peekValueStack :: TIM Value
peekValueStack = do
  vstack <- gets tim_value_stack
  case Stack.peek vstack of
    Nothing -> do
      throwTIMError "peekValueStack: empty value stack"
    Just value -> do
      return value

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

----------------------------------------
-- Frames

-- Set the current frame pointer
setCurrentFramePtr :: FramePtr -> TIM ()
setCurrentFramePtr ptr = modify' $ \st ->
  st { tim_curr_frame = ptr }

isCurrentFramePartial :: TIM Bool
isCurrentFramePartial = do
  frame_is_partial <$> getCurrentFrame

-- Fetches the current frame
getCurrentFrame :: TIM Frame
getCurrentFrame = do
  manipulateCurrentFrame Just

-- Update a closure in the current frame
updateCurrentFrameSlot :: Offset -> Closure -> TIM ()
updateCurrentFrameSlot offset closure = void $ do
  manipulateCurrentFrame (updateFrame offset closure)

-- Generic frame manipulations

-- Fetches a frame and updates it with the given function.
-- Returns the updated frame or an error if anything goes wrong.
manipulateFrameFromAddr :: Addr -> (Frame -> Maybe Frame) -> TIM Frame
manipulateFrameFromAddr addr f = do
  heap <- gets tim_heap
  case Heap.deref addr heap of
    Nothing -> do
      throwTIMError "manipulateFrameFromAddr: invalid frame address"
    Just frame -> do
      newFrame <- do
        case f frame of
          Nothing -> do
            throwTIMError "manipulateFrameFromAddr: update to frame failed (possible wrong offset)"
          Just frame' -> do
            return frame'
      let newHeap = fromJust (Heap.update (Just . const newFrame) addr heap)
      modify' $ \st -> st { tim_heap = newHeap }
      return newFrame

-- Fetches the current frame and updates it with the given function.
-- Returns the updated current frame or an error if anything goes wrong.
manipulateCurrentFrame :: (Frame -> Maybe Frame) -> TIM Frame
manipulateCurrentFrame f = do
  framePtr <- gets tim_curr_frame
  case framePtr of
    AddrP addr -> do
      manipulateFrameFromAddr addr f
    _ -> do
      throwTIMError "manipulateCurrentFrame: the current frame address is not to a frame pointer"

manipulateFramePtr :: FramePtr -> (Frame -> Maybe Frame) -> TIM Frame
manipulateFramePtr fp f = do
  case fp of
    AddrP addr -> do
      manipulateFrameFromAddr addr f
    _ -> do
      throwTIMError "derefFramePtr: the frame pointer is not to a frame"

----------------------------------------
-- Heap operations

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

----------------------------------------
-- Utilities

-- Throw an error
throwTIMError :: Text -> TIM a
throwTIMError msg = throwError (TIMError msg)

-- Log the current TIM state
logTIMState :: TIM ()
logTIMState = get >>= tell . TIMTrace . pure

-- Lookup for a primitive operation
lookupPrim :: Name -> TIM Prim
lookupPrim name = do
  case Map.lookup name primitives of
    Nothing -> do
      throwTIMError ("lookupPrim: primitive " <> fromName name <> " does not exist")
    Just prim -> do
      return prim