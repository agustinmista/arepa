module Language.TIM.Interpreter.Types where

import Data.Heap

import Prettyprinter

import Language.TIM.Syntax

----------------------------------------
-- TIM interpreter types
----------------------------------------

----------------------------------------
-- Frames and frame pointers

data FramePtr = NullP | AddrP Addr | ValueP Value
  deriving (Show, Read, Eq, Ord)

instance Pretty FramePtr where
  pretty NullP = "null"
  pretty (AddrP addr) = viaShow addr
  pretty (ValueP value) = pretty value

data Frame = Frame {
  frame_size :: Int,
  frame_is_partial :: Bool,
  frame_closures :: [Closure]
} deriving (Show, Read, Eq, Ord)

instance Pretty Frame where
  pretty frame = vsep $
    [ "Frame:" ] <>
    if null (frame_closures frame)
      then [ indent 2 "empty" ]
      else [ indent 2 ("$" <> pretty i <> ":" <+> pretty closure)
           | (closure, i) <- reverse (zip (frame_closures frame) ([0..] :: [Int]))
           ]

mkFrame :: [Closure] -> Frame
mkFrame closures = Frame {
  frame_size = length closures,
  frame_closures = closures,
  frame_is_partial = False
}

mkPartialFrame :: [Closure] -> Frame
mkPartialFrame closures = Frame {
  frame_size = length closures,
  frame_closures = closures,
  frame_is_partial = True
}

-- Offsets withing a frame
type Offset = Int

manipulateFrame :: Offset -> (Closure -> Closure) -> Frame -> Maybe Frame
manipulateFrame offset f frame
  | offset >= 0 && offset < frame_size frame =
      Just frame {
        frame_closures =
          take offset (frame_closures frame) <>
          [f $ frame_closures frame !! offset] <>
          drop (offset+1) (frame_closures frame)
      }
  | otherwise = Nothing

updateFrame :: Offset -> Closure -> Frame -> Maybe Frame
updateFrame offset closure = manipulateFrame offset (const closure)

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

dummyClosure :: Closure
dummyClosure = Closure [] NullP