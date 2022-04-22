module Language.TIM.Prim.Types where

import Data.Map (Map)
import Data.Map qualified as Map

import Language.TIM.Syntax

----------------------------------------
-- Primitive operations
----------------------------------------

-- Primitive operations

data PrimOp = PrimOp {
  prim_arity :: Int,
  prim_runner :: [Value] -> IO Value
}

type PrimMap = Map Name PrimOp

isPrimOp :: Name -> PrimMap -> Bool
isPrimOp name prims = name `elem` Map.keys prims
