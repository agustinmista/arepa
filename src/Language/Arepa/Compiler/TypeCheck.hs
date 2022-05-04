module Language.Arepa.Compiler.TypeCheck
  ( typeCheckModule
  ) where

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad

----------------------------------------
-- Type checking
----------------------------------------

typeCheckModule :: MonadArepa m => CoreModule -> m CoreModule
typeCheckModule m = do
  let name = mod_name m
  whenVerbose $ debug ("Type checking module " <> prettyPrint name)
  return m

----------------------------------------
-- The type checking monad

-- TODO