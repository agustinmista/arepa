module Language.TIM.Syntax
  ( module Language.TIM.Syntax
  , Name
  , mkName
  , fromName
  ) where

import GHC.Exts
import Data.List

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Text.Lazy (Text)

import Prettyprinter

import Language.Arepa.Syntax (Name, mkName, fromName)

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
      hcat (intersperse comma (pretty <$> instrs))

isNullCodeBlock :: CodeBlock -> Bool
isNullCodeBlock (CodeBlock instrs) = null instrs

splitCodeBlock :: CodeBlock -> (Instr, CodeBlock)
splitCodeBlock (CodeBlock instrs)
  | null instrs = error "splitCodeBlock: empty code"
  | otherwise   = (head instrs, CodeBlock (tail instrs))

-- Instructions

data Instr =
    TakeArgI Int
  | EnterI ArgMode
  | PushArgI ArgMode
  | PushValueI ValueMode
  | CallI Name
  | ReturnI
  deriving (Show, Read, Eq, Ord)

instance Pretty Instr where
  pretty (TakeArgI n) =
    "take" <+> pretty n
  pretty (EnterI mode) =
    "enter" <+> pretty mode
  pretty (PushArgI mode) =
    "push arg" <+> pretty mode
  pretty (PushValueI mode) =
    "push value" <+> pretty mode
  pretty (CallI prim) =
    "call" <+> pretty prim
  pretty ReturnI =
    "return"

-- Argument addressing modes

data ArgMode =
    ArgM Int
  | LabelM Name
  | ValueM Value
  deriving (Show, Read, Eq, Ord)

instance Pretty ArgMode where
  pretty (ArgM n) =
    "$" <> pretty n
  pretty (LabelM var) =
    pretty var
  pretty (ValueM value) =
    pretty value

-- Values (akin to literals, but not always the same)

data Value =
    IntV Int
  | DoubleV Double
  | CharV Char
  | StringV Text
  | VoidV ()
  deriving (Show, Eq, Read, Ord)

instance Pretty Value where
  pretty (IntV n)    = angles (pretty n)
  pretty (DoubleV n) = angles (pretty n)
  pretty (CharV c)   = angles (pretty (show c))
  pretty (StringV s) = angles (pretty (show s))
  pretty (VoidV _)   = angles "void"

-- Values addressing modes

data ValueMode =
    FramePtrM
  | InlineM Value
  deriving (Show, Eq, Read, Ord)

instance Pretty ValueMode where
  pretty FramePtrM =
    "fp"
  pretty (InlineM value) =
    "inline" <+> pretty value

-- Value types

data Type =
    IntT
  | DoubleT
  | CharT
  | StringT
  | VoidT
  deriving (Show, Eq, Read, Ord)

instance Pretty Type where
  pretty IntT    = "Int"
  pretty DoubleT = "Double"
  pretty CharT   = "Char"
  pretty StringT = "String"
  pretty VoidT   = "Void"

-- Map values to types

valueType :: Value -> Type
valueType IntV    {} = IntT
valueType DoubleV {} = DoubleT
valueType CharV   {} = CharT
valueType StringV {} = StringT
valueType VoidV   {} = VoidT