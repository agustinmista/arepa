module Language.TIM.Interpreter.Types where

import Data.Heap

import Prettyprinter

import Language.TIM.Syntax

----------------------------------------
-- TIM interpreter types
----------------------------------------

----------------------------------------
-- Frames and frame pointers

data FramePtr = NullP | AddrP Addr | LitP Lit
  deriving (Show, Read, Eq, Ord)

instance Pretty FramePtr where
  pretty NullP = "null"
  pretty (AddrP addr) = viaShow addr
  pretty (LitP lit) = angles (pretty lit)

litCode :: CodeBlock
litCode = []

data Frame = Frame {
  frame_size :: Int,
  frame_closures :: [Closure]
} deriving (Show, Read, Eq, Ord)

instance Pretty Frame where
  pretty frame = vsep $
    [ "Frame:" ] <>
    if null (frame_closures frame)
      then [ indent 2 "empty" ]
      else [ indent 2 (pretty closure) | closure <- reverse (frame_closures frame) ]

mkFrame :: [Closure] -> Frame
mkFrame closures = Frame {
  frame_size = length closures,
  frame_closures = closures
}

-- Offsets withing a frame
type Offset = Int

updateFrame :: Offset -> Closure -> Frame -> Maybe Frame
updateFrame offset closure frame
  | offset > 0 && offset < frame_size frame =
      Just frame {
        frame_closures =
          take (offset-1) (frame_closures frame) <>
          [closure] <>
          drop offset (frame_closures frame)
      }
  | otherwise = Nothing

frameOffset :: Int -> Frame -> Maybe Closure
frameOffset offset frame
  | offset >= 0 && offset < frame_size frame =
      Just (frame_closures frame !! offset)
  | otherwise =
      Nothing

----------------------------------------
-- Closures

data Closure = Closure {
  closure_code :: CodeBlock,
  closure_frame :: FramePtr
} deriving (Show, Read, Eq, Ord)

instance Pretty Closure where
  pretty closure =
    "closure" <+>
    parens (
      pretty (closure_frame closure) <>
      comma <>
      pretty (closure_code closure)
    )

mkClosure :: CodeBlock -> FramePtr -> Closure
mkClosure = Closure
