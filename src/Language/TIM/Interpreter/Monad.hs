module Language.TIM.Interpreter.Monad where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import Prettyprinter

import Data.Text.Lazy (Text)

import Data.Stack (Stack)
import Data.Stack qualified as Stack

import Data.Heap (Heap)
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

runTIM :: FilePath -> CodeStore -> TIM a -> IO (Either TIMError a, TIMTrace)
runTIM stdout code (TIM ma) = withRTSStdout stdout $ do
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

-- Machine state

data TIMState = TIMState {
  tim_code_store :: CodeStore,
  tim_curr_codeblock :: CodeBlock,
  tim_curr_frame :: FramePtr,
  tim_heap :: Heap Frame,
  tim_arg_stack :: Stack Closure,
  tim_arg_dump :: (),
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
        vsep $
          [ "Argument dump:" ] <>
          [ indent 2 (pretty (tim_arg_dump st)) ]
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
  tim_arg_dump = (),
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

-- Update a closure in the current frame
updateFrameSlot :: Offset -> Closure -> TIM ()
updateFrameSlot offset closure = do
  st <- get
  let curr_frame = tim_curr_frame st
  case curr_frame of
    AddrP addr -> do
      case Heap.update (updateFrame offset closure) addr (tim_heap st) of
        Nothing -> do
          throwTIMError "updateClosure: invalid frame address or frame offset"
        Just heap -> do
          put st { tim_heap = heap }
    _ -> do
      throwTIMError "updateClosure: updating a closure in the current frame requires an address frame pointer"
