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
