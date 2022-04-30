{-# LANGUAGE ForeignFunctionInterface #-}
module Language.TIM.Prim
  ( primitives
  , module Language.TIM.Prim.Types
  ) where

import System.FilePath

import Language.TIM.Prim.Types
import Language.TIM.Prim.TH

----------------------------------------
-- Mining primitive operations from the RTS

mkPrimitives "primitives" ("rts" </> "include" </> "prim.h")