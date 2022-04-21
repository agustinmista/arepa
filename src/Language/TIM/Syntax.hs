module Language.TIM.Syntax
  ( module Language.TIM.Syntax
  , Name
  , fromName
  , Lit
  ) where

import GHC.Exts
import Data.List

import Data.Map (Map)
import Data.Map qualified as Map

import Prettyprinter

import Language.Arepa.Syntax (Name, fromName, Lit)

----------------------------------------
-- TIM: Three instruction machine
----------------------------------------

-- CodeBlock stores (somewhat equivalent to core modules)

data CodeStore = CodeStore {
  store_name :: Name,
  store_blocks :: Map Name CodeBlock
} deriving (Show, Read, Eq, Ord)

instance Pretty CodeStore where
  pretty store =
    vsep $
      [ "store" <+> pretty (store_name store) <+> lbrace ] <>
      [ vsep [pretty name <> ":", indent 2 (vsep (pretty <$> toList instrs))]
      | (name, instrs) <- Map.toList (store_blocks store) ] <>
      [ rbrace ]

emptyCodeStore :: Name -> CodeStore
emptyCodeStore name = CodeStore {
  store_name = name,
  store_blocks = mempty
}

codeStoreName :: CodeStore -> Name
codeStoreName = store_name

lookupCodeStore :: Name -> CodeStore -> Maybe CodeBlock
lookupCodeStore v store = Map.lookup v (store_blocks store)

insertCodeStore :: Name -> CodeBlock -> CodeStore -> CodeStore
insertCodeStore v code store = store {
  store_blocks = Map.insert v code (store_blocks store)
}

-- CodeBlock

newtype CodeBlock = CodeBlock [Instr]
  deriving (Show, Read, Eq, Ord)
  deriving Semigroup via [Instr]
  deriving Monoid    via [Instr]

instance IsList CodeBlock where
  type Item CodeBlock = Instr
  fromList = CodeBlock
  toList (CodeBlock instrs) = instrs

instance Pretty CodeBlock where
  pretty (CodeBlock instrs) =
    brackets $
      cat (intersperse comma (pretty <$> instrs))

isNullCodeBlock :: CodeBlock -> Bool
isNullCodeBlock (CodeBlock instrs) = null instrs

splitCodeBlock :: CodeBlock -> (Instr, CodeBlock)
splitCodeBlock (CodeBlock instrs)
  | null instrs = error "splitCodeBlock: empty code"
  | otherwise   = (head instrs, CodeBlock (tail instrs))

entryPoint :: CodeBlock
entryPoint = CodeBlock [ EnterI (LabelM "main") ]

-- Instructions

data Instr =
    TakeI Int
  | EnterI AddressMode
  | PushI AddressMode
  deriving (Show, Read, Eq, Ord)

instance Pretty Instr where
  pretty (TakeI n) =
    "take" <+> pretty n
  pretty (EnterI mode) =
    "enter" <+> pretty mode
  pretty (PushI mode) =
    "push" <+> pretty mode

-- Instruction addressing modes

data AddressMode =
    ArgM Int
  | LabelM Name
  | LitM Lit
  deriving (Show, Read, Eq, Ord)

instance Pretty AddressMode where
  pretty (ArgM n) =
    "$" <> pretty n
  pretty (LabelM var) =
    pretty var
  pretty (LitM lit) =
    pretty lit