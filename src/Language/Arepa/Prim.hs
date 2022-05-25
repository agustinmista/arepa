module Language.Arepa.Prim
  ( pattern CallE
  , isCallE
  , pattern NonPrimVarE
  , isNonPrimVarE
  , module Language.TIM.Prim
  ) where

import Language.Arepa.Syntax
import Language.TIM.Prim

----------------------------------------
-- Primitive operations (reexported from TIM)
----------------------------------------

-- A pattern to identify calls to primitive operations

pattern CallE :: Name -> [CoreExpr] -> CoreExpr
pattern CallE name args <- (isCallE -> Just (name, args))
  where CallE name args = foldl AppE (VarE name) args

isCallE :: CoreExpr -> Maybe (Name, [CoreExpr])
isCallE expr =
  case collectArgs expr of
    (VarE name, args) | name `isPrimOp` primitives -> Just (name, args)
    _ -> Nothing

-- A pattern to avoid overlapping with CallE in VarE

pattern NonPrimVarE :: Name -> CoreExpr
pattern NonPrimVarE name <- (isNonPrimVarE -> Just name)
  where NonPrimVarE name = VarE name

isNonPrimVarE :: CoreExpr -> Maybe Name
isNonPrimVarE expr =
  case expr of
    VarE name | not (name `isPrimOp` primitives) -> Just name
    _ -> Nothing
