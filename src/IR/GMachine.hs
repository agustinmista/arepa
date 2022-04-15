module IR.GMachine where

import Syntax

import Data.Map (Map)
import Data.List (mapAccumL)
import Data.Map qualified as Map

------------------
-- Instructions --
------------------
data GmInst = Unwind
            | PushGlobal Var
            | PushInt Int
            | Push Int
            | MkApp
            | Slide Int
  deriving (Show, Eq)

-- | A G-Machine program is just a list of instructions
type GmCode = [GmInst]

-- | A G-Machine operates on a graph represented by
--   the following nodes in a stack
data GmNode = NNum Int
            | NApp Addr Addr
            | NGlobal Int GmCode
  deriving (Show , Eq)

------------------
-- Type aliases --
------------------
type Addr   = Int

type GmStack = [Addr]

type GmHeap = Map Addr GmNode

type GmGlobals = Map Var Addr

type Offset = Int

type GmEnv = Map Var Offset

-- | A G-Machine is composed of the `code` running,
--   a `stack` on which the machine operates,
--   a `heap` where all graph nodes are allocated,
--   and a `globals` mapping from function names to
--   their corresponding graph representation (and code)
data GmState = GmState {
  code    :: GmCode,
  stack   :: GmStack,
  heap    :: GmHeap,
  globals :: GmGlobals
} deriving Show

-----------------------
-- Utility functions --
-----------------------

--TODO: Can this be improved?
updateHeap :: GmState -> (GmHeap -> GmHeap) -> GmState
updateHeap state f = state {heap = f $ heap state}

-- | Pop the stack inside the G-Machine state
popGm :: GmState -> (GmState,Addr)
popGm state = (state { stack = rest }, addr)
  where addr:rest =
          case stack state of
            [] -> error "ERROR: The stack is empty!"
            l  -> l

-- | Drop the firs element in the stack inside the G-Machine state
dropGm :: GmState -> GmState
dropGm = fst . popGm

-- | Add an element at the top of the stack inside the G-Machine state
pushGm :: GmState -> Addr -> GmState
pushGm state addr = state { stack = addr : stack state }

-- | Looks for a address in the heap and fails if it can not find it
hLookupGm :: GmHeap -> Addr -> GmNode
hLookupGm h addr =
  case Map.lookup addr h of
    Nothing   -> error $ "ERROR: Address " ++ show addr ++ " does not exists"
    Just node -> node

-- | Allocates a new node in the heap on a fresh address
hAllocGm :: GmNode -> GmHeap -> (Addr,GmHeap)
hAllocGm node h = (free,Map.insert free node h)
  where
    free = head $ filter (`Map.notMember` h) [1..]

addOffset :: Offset -> GmEnv -> GmEnv
addOffset i = Map.map (+i)

---------------------------
-- G-Machine interpreter --
---------------------------

-- | Given a state returns runs the G-Machine and returns a list with
--   each intermediate state with `last . evalGm` being the final result
--   (if any)
evalGmStates :: GmState -> [GmState]
evalGmStates state = state : restState
  where
    restState | gmFinal (code state) = []
              | otherwise            = evalGmStates nextState
    nextState  = stepGm state
    gmFinal [] = True
    gmFinal _  = False

-- | Run a terminating G-Machine and return the single remaining
--   node in the stack
evalGm :: GmState -> GmNode
evalGm state = hLookupGm (heap lastState) . head . stack $ lastState
  where
    lastState = last $ evalGmStates state

-- | A single instruction transition from the current state to the next one
stepGm :: GmState -> GmState
stepGm state = doInst i (state {code = is})
  where
    i:is = code state
    doInst  Unwind        = doUnwindGm
    doInst (PushGlobal s) = doPushGlobalGm s
    doInst (PushInt n)    = doPushIntGm n
    doInst (Push n)       = doPushGm n
    doInst  MkApp         = doAppGm
    doInst (Slide n)      = doSlideGm n

-- | Fetch the address of the given function from `globals` and
--   place it at the top of the stack
doPushGlobalGm :: Var  -> GmState -> GmState
doPushGlobalGm s state =
  case Map.lookup s (globals state) of
    Nothing  -> error $ "ERROR: Function " ++ show s ++ " does not exists"
    Just n   -> state { stack = n : stack state }

-- | Allocate some heap space for an integer an
--   place it at the top of the stack
doPushIntGm :: Int -> GmState -> GmState
doPushIntGm i state = pushGm newState addr
  where (addr,h) = hAllocGm (NNum i) (heap state)
        newState = state { heap = h }


-- | Place the argument at the given offset
--   (starting  from 0 before the top of the stack)
--   and place it at the top of the stack. It must be the
--   case that the node at the given offset is an application and as
--   such we take its operand (the argument).
doPushGm :: Int -> GmState -> GmState
doPushGm n state =
  case hLookupGm h addr of
    NApp _ argN -> pushGm state argN
    _           -> error $ "ERROR: no application at " ++ show (n+1,addr)
  where
    h  = heap state
    s  = stack state
    addr = if n + 1 < length s
           then  s !! n + 1
           else error "ERROR: There are not enough argument in the stack"

-- | Creates a application from the two topmost elements of the stack
--   as a operator (1st element) applied to an operant (2nd element)
--   removing them from the stack
doAppGm :: GmState -> GmState
doAppGm state = state { stack = a:rest, heap = h}
  where
    a1:a2:rest = stack state
    (a,h)      = hAllocGm (NApp a1 a2) $ heap state

-- | Takes the top of the stack and slides it a given offset.
--   This occurs when we must replace a function application in the stack
--   with its graph representation.
doSlideGm :: Int -> GmState -> GmState
doSlideGm n state = state { stack = head s : drop n (tail s)}
  where s = stack state

-- | Unwind takes the topmost element in the stack determines what to do next:
--   (NNum) Consume the instruction and return the state (We are done)
--   (NApp) Unfold the application and push the operator to the stack
--   (NGlobal) At this point all argument for the function call should be on
--             the stack, check for this and start executing the function code
--
-- Note: it is assumed tha Unwind is the last instruction in any program.
doUnwindGm :: GmState -> GmState
doUnwindGm state = newState (hLookupGm h a)
  where
    -- Current Heap & Stack
    a:as = stack state
    h    = heap state
    -- What to do depending on what is in the top of the stack
    newState (NNum _)         = state { code = [] }
    newState (NApp a1 _)     = pushGm state a1
    newState (NGlobal d fCode)
      | length  as < d = error "ERROR: Not enough arguments"
      | otherwise      = state { code = fCode }

------------------------
-- G-Machine compiler --
------------------------

type GmCompDefs = (Var,Int,GmCode)

compiler :: CoreModule -> GmState
compiler m = GmState {code=initialCode,
                      stack=[],
                      heap=startHeap,
                      globals=Map.fromList startGlobals}
  where
    initialCode              = [PushGlobal"main",Unwind]
    (startHeap,startGlobals) = mapAccumL allocateSC Map.empty compiled
    compiled                 = map compilerSC (mod_binds m)

allocateSC :: GmHeap -> GmCompDefs -> (GmHeap,(Var,Addr))
allocateSC aHeap (fName,nArgs,fCode) = (updatedHeap, (fName,addr))
  where
    (addr,updatedHeap) = hAllocGm (NGlobal nArgs fCode) aHeap

compilerSC :: CoreDecl -> GmCompDefs
compilerSC (FunD fName args body) = (fName,length args,compilerR body env)
  where
    env = Map.fromList $ zip args [0..]
compilerSC (ValD _ _) =
  error "TODO: value definitions are not supported"

compilerR :: CoreExpr -> GmEnv -> GmCode
compilerR e env = compilerC e env ++ [Slide $ length env + 1, Unwind]

compilerC :: CoreExpr -> GmEnv -> GmCode
compilerC (VarE v) env =
  case Map.lookup v env of
    Nothing -> [PushGlobal v]
    Just n  -> [Push n]
compilerC (LitE lit) _ =
  case lit of
    IntL n -> [PushInt n]
    _      -> error $ "TODO: " ++ show lit ++ " not implemented yet"
compilerC (AppE e1 e2) env = compilerC e1 env ++
                             compilerC e2 (addOffset 1 env) ++
                             [MkApp]
compilerC _ _ = error "TODO: This expression is not supported yet"
