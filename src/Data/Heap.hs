module Data.Heap
  ( Addr
  , nullAddr
  , Heap
  , empty
  , size
  , alloc
  , update
  , free
  , deref
  ) where

import Data.Map (Map)
import Data.Map qualified as Map

----------------------------------------
-- Heaps (as in memory heaps)
----------------------------------------

-- Addresses

newtype Addr = Addr Int
  deriving (Read, Eq, Ord)

instance Show Addr where
  show (Addr n) = "#" <> show n

nullAddr :: Addr
nullAddr = Addr 0

-- Heaps

data Heap a = Heap {
  heap_size :: Int,              -- ^ The number of objects in the heap
  heap_free_addrs :: [Addr],     -- ^ The available addresses (infinite)
  heap_used_addrs :: Map Addr a  -- ^ The allocated addreses
}

-- Create an empty heap with unbounded memory
empty :: Heap a
empty = Heap {
  heap_size = 0,
  heap_free_addrs = Addr <$> [1..],
  heap_used_addrs = Map.empty
}

-- Return the heap size
size :: Heap a -> Int
size = heap_size

-- Allocate an object
alloc :: a -> Heap a -> (Addr, Heap a)
alloc a heap =
  let addr = head (heap_free_addrs heap) in
  ( addr
  , heap {
      heap_size = heap_size heap + 1,
      heap_free_addrs = tail (heap_free_addrs heap),
      heap_used_addrs = Map.insert addr a (heap_used_addrs heap)
    }
  )

-- Update an allocated object
-- This can return `Nothing` in two cases:
-- * If the input address is invalid
-- * If the update function returns `Nothing`
update :: (a -> Maybe a) -> Addr -> Heap a -> Maybe (Heap a)
update f addr heap = do
  obj <- Map.lookup addr (heap_used_addrs heap)
  obj' <- f obj
  return heap {
    heap_used_addrs = Map.insert addr obj' (heap_used_addrs heap)
  }

-- Free an object from the heap
free :: Addr -> Heap a -> Heap a
free addr heap = heap {
    heap_size = heap_size heap - 1,
    heap_free_addrs = addr : heap_free_addrs heap,
    heap_used_addrs = Map.delete addr (heap_used_addrs heap)
  }

-- Lookup for an allocated object
deref :: Addr -> Heap a -> Maybe a
deref addr heap = Map.lookup addr (heap_used_addrs heap)