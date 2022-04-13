module Language.Arepa.Compiler.TypeCheck 
  ( typeCheckModule
  ) where

import Language.Arepa.Compiler.Monad
import Language.Arepa.Syntax

----------------------------------------
-- Type checking
----------------------------------------

typeCheckModule :: CoreModule -> Compiler CoreModule 
typeCheckModule = return -- no type-checking at the moment