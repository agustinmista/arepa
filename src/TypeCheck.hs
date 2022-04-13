module TypeCheck 
  ( typeCheckModule
  ) where

import Syntax
import Compiler

----------------------------------------
-- Type checking
----------------------------------------

typeCheckModule :: CoreModule -> Compiler CoreModule 
typeCheckModule = return -- no type-checking at the moment