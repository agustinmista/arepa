module Language.Arepa.Compiler.TypeCheck
  ( typeCheckModule
  ) where

import Language.Arepa.Syntax
import Language.Arepa.Compiler.Monad

----------------------------------------
-- Type checking
----------------------------------------

typeCheckModule :: MonadArepa m => CoreModule -> m CoreModule
typeCheckModule = return -- no type-checking at the moment

----------------------------------------
-- The type checking monad

-- TODO