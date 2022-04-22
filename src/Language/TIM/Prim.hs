{-# LANGUAGE ForeignFunctionInterface #-}
module Language.TIM.Prim where

import Data.Map (Map)
import Data.Map qualified as Map

import Language.TIM.Syntax

----------------------------------------
-- Primitive operations
----------------------------------------

data PrimOp = PrimOp {
  prim_arity :: Int,
  prim_runner :: [Value] -> IO Value
}

isPrimOp :: Name -> Bool
isPrimOp name = name `elem` Map.keys timPrimitives

----------------------------------------
-- Mining primitive operations from the RTS

timPrimitives :: Map Name PrimOp
timPrimitives = Map.fromList [
    ("__arepa_rts_add_int__", PrimOp 2 $ \[IntV x, IntV y] -> IntV <$> c__arepa_rts_add_int__ x y),
    ("__arepa_rts_sub_int__", PrimOp 2 $ \[IntV x, IntV y] -> IntV <$> c__arepa_rts_sub_int__ x y),
    ("__arepa_rts_pi__",      PrimOp 0 $ \[]               -> DoubleV <$> c__arepa_rts_pi__)
  ]

foreign import ccall "__arepa_rts_add_int__" c__arepa_rts_add_int__ :: Int -> Int -> IO Int
foreign import ccall "__arepa_rts_sub_int__" c__arepa_rts_sub_int__ :: Int -> Int -> IO Int
foreign import ccall "__arepa_rts_pi__"      c__arepa_rts_pi__      :: IO Double