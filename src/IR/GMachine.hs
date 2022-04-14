module IR.GMachine where

import           Data.Map (Map)
import qualified Data.Map as Map

------------------
-- Instructions --
------------------
data GmInst = Unwind
            | PushGlobal String
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

------------------
-- Type aliases --
------------------
type Addr   = Int

type GmStack = [Addr]

type GmHeap = Map Addr GmNode

type GmGlobals = Map String Addr

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
}
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
hAllocGm :: GmNode -> GmHeap -> GmHeap
hAllocGm node h = Map.insert free node h
  where
    free = head $ filter (flip Map.notMember h) [1..]

