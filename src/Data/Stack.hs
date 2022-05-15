module Data.Stack
  ( Stack
  , empty
  , size
  , push
  , peek
  , pop
  , take
  , isEmpty
  , fromList
  , toList
  , append
  ) where

import Prelude hiding (take)
import Prelude qualified

----------------------------------------
-- Stack (a.k.a. lists)
----------------------------------------

data Stack a = Stack {
  stack_size :: Int,   -- ^ The number of objects in the stack
  stack_values :: [a]  -- ^ The objects in the stack
}

-- Create an empty heap with unbounded memory
empty :: Stack a
empty = Stack {
  stack_size = 0,
  stack_values = []
}

-- Return the heap size
size :: Stack a -> Int
size = stack_size

-- Allocate an object
push :: a -> Stack a -> Stack a
push a stack =
  stack {
    stack_size = stack_size stack + 1,
    stack_values = a : stack_values stack
  }

-- Peek the top element of the stack (without removing it)
peek :: Stack a -> Maybe a
peek stack
  | stack_size stack == 0 =
      Nothing
  | otherwise =
      Just (head (stack_values stack))

-- Remove the top element of the stack
pop :: Stack a -> Maybe (a, Stack a)
pop stack
  | stack_size stack == 0 =
      Nothing
  | otherwise =
      Just (
        head (stack_values stack),
        stack {
          stack_size = stack_size stack - 1,
          stack_values = tail (stack_values stack)
        }
      )

-- Take the top elements from the stack in one go
take :: Int -> Stack a -> Maybe ([a], Stack a)
take n stack
  | stack_size stack < n =
      Nothing
  | otherwise =
      Just (
        Prelude.take n (stack_values stack),
        stack {
          stack_size = stack_size stack - n,
          stack_values = drop n (stack_values stack) }
      )

-- Create a stack from a peasant list
fromList :: [a] -> Stack a
fromList xs = Stack {
  stack_size = length xs,
  stack_values = xs
}

-- Forget the stack structure
toList :: Stack a -> [a]
toList = stack_values

-- Append two stacks
append :: Stack a -> Stack a -> Stack a
append stk1 stk2 = Stack {
  stack_size = stack_size stk1 + stack_size stk2,
  stack_values = stack_values stk1 <> stack_values stk2
}

isEmpty :: Stack a -> Bool
isEmpty  = null . toList