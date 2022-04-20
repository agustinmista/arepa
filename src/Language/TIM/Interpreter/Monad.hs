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

import Language.TIM.Syntax
import Language.TIM.Interpreter.Types

----------------------------------------
-- TIM interpreter monad
----------------------------------------

-- TIM monad

newtype TIM a = TIM (ExceptT TIMError (StateT TIMState (WriterT [TIMState] IO)) a)
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadError TIMError
           , MonadState TIMState
           , MonadWriter [TIMState] )

runTIM :: CodeStore -> TIM a -> IO (Either TIMError a, [TIMState])
runTIM code (TIM ma) = runWriterT (evalStateT (runExceptT ma) (initialTIMState code))

-- Machine exceptions

newtype TIMError = TIMError Text

instance Pretty TIMError where
  pretty (TIMError err) = pretty err

-- Machine state

data TIMState = TIMState {
  tim_code_store :: CodeStore,
  tim_curr_codeblock :: CodeBlock,
  tim_curr_frame :: FramePtr,
  tim_heap :: Heap Frame,
  tim_arg_stack :: Stack Closure,
  tim_arg_dump :: (),
  tim_value_stack :: ()
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
        vsep $
          [ "Argument dump:" ] <>
          [ indent 2 (pretty (tim_arg_dump st)) ]
      showValueStack =
        vsep $
          [ "Value stack:" ] <>
          [ indent 2 (pretty (tim_value_stack st)) ]

initialTIMState :: CodeStore -> TIMState
initialTIMState code = TIMState {
  tim_code_store = code,
  tim_curr_codeblock = entryPoint,
  tim_curr_frame = NullP,
  tim_heap = Heap.empty,
  tim_arg_stack = Stack.empty,
  tim_arg_dump = (),
  tim_value_stack = ()
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
logTIMState = get >>= tell . pure

-- Lookup the code of a compiled label
lookupCodeBlock :: Name -> TIM CodeBlock
lookupCodeBlock v = do
  store <- gets tim_code_store
  case lookupCodeStore v store of
    Nothing -> throwTIMError ("lookupCodeBlock: variable " <> fromName v <> " not in the store")
    Just code -> return code

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
pushStack :: Closure -> TIM ()
pushStack closure = modify' $ \st ->
  st { tim_arg_stack = Stack.push closure (tim_arg_stack st) }

-- Take the first `n` elements from the current stack
takeStack :: Int -> TIM [Closure]
takeStack n = do
  st <- get
  case Stack.take n (tim_arg_stack st) of
    Nothing -> throwTIMError "takeStack: not enough elements"
    Just (closures, stack) -> do
      put st { tim_arg_stack = stack }
      return closures

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
derefClosure :: AddressMode -> TIM Closure
derefClosure mode = do
  st <- get
  let heap = tim_heap st
  let curr_frame = tim_curr_frame st
  case mode of
    ArgM offset -> do
      case curr_frame of
        AddrP addr ->
          case Heap.deref addr heap of
            Nothing -> throwTIMError "derefClosure: cannot find frame"
            Just frame -> do
              case frameOffset offset frame of
                Nothing -> throwTIMError "derefClosure: invalid frame offset"
                Just closure -> return closure
        _ -> do
          throwTIMError "derefClosure: dereferencing an argument offset requires an address frame pointer"
    LabelM v -> do
      code <- lookupCodeBlock v
      return (mkClosure code curr_frame)
    LitM lit -> do
      return (mkClosure litCode (LitP lit))

-- Update a closure in the heap
updateClosure :: Addr -> Offset -> Closure -> TIM ()
updateClosure addr offset closure = do
  st <- get
  case Heap.update (updateFrame offset closure) addr (tim_heap st) of
    Nothing -> throwTIMError "updateClosure: invalid frame address or frame offset"
    Just heap -> put st { tim_heap = heap }