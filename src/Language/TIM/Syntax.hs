module Language.TIM.Syntax
  ( module Language.TIM.Syntax
  , module Data.Name
  ) where

import GHC.Exts
import Data.List

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Text.Lazy (Text)

import Prettyprinter

import Data.Name

import Language.TIM.Types

----------------------------------------
-- TIM syntax
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
    TakeArgI Int Int
  | PushArgI ArgMode
  | PushValueI ValueMode
  | PushMarkerI Int
  | UpdateMarkersI Int
  | EnterI ArgMode
  | MoveI Int ArgMode
  | ReturnI
  | CallI Name
  | DataI Tag
  | SwitchI (Map Tag Label)
  deriving (Show, Read, Eq, Ord)

type Label = Name
type Tag = Int

instance Pretty Instr where
  pretty (TakeArgI t n) =
    "take" <+> pretty t <+> pretty n
  pretty (PushArgI mode) =
    "push arg" <+> pretty mode
  pretty (PushValueI mode) =
    "push value" <+> pretty mode
  pretty (PushMarkerI n) =
    "push marker" <+> pretty n
  pretty (UpdateMarkersI n) =
    "update markers" <+> pretty n
  pretty (EnterI mode) =
    "enter" <+> pretty mode
  pretty (MoveI n mode) =
    "move" <+> pretty n <+> pretty mode
  pretty ReturnI =
    "return"
  pretty (CallI prim) =
    "call" <+> pretty prim
  pretty (DataI tag) =
    "con" <+> pretty tag
  pretty (SwitchI alts) =
    "switch" <+>
    brackets (hcat (intersperse ","
      [ "tag" <+> pretty tag <+> "=>" <+> pretty mode
      | (tag, mode) <- Map.toList alts
      ]))

-- Argument addressing modes

type Offset = Int

data ArgMode =
    ArgM Offset
  | ValueM Value
  | LabelM Label
  | DataM Offset
  deriving (Show, Read, Eq, Ord)

instance Pretty ArgMode where
  pretty (ArgM n) =
    "$" <> pretty n
  pretty (ValueM value) =
    "value" <+> pretty value
  pretty (LabelM var) =
    "label" <+> pretty var
  pretty (DataM tag) =
    "data" <+> pretty tag

-- Values (akin to literals, but not always the same)

data Value =
    IntV Int
  | DoubleV Double
  | StringV Text
  | VoidV ()
  | TagV Tag
  deriving (Show, Read, Eq, Ord)

instance Pretty Value where
  pretty (IntV n)    = angles (pretty n)
  pretty (DoubleV n) = angles (pretty n)
  pretty (StringV s) = angles (pretty (show s))
  pretty (VoidV _)   = angles "void"
  pretty (TagV n)   =  angles ("tag" <+> pretty n)

-- Values addressing modes

data ValueMode =
    FramePtrM
  | InlineM Value
  deriving (Show, Read, Eq, Ord)

instance Pretty ValueMode where
  pretty FramePtrM =
    "fp"
  pretty (InlineM value) =
    "inline" <+> pretty value

-- Map values to types

typeOfValue :: Value -> Type
typeOfValue IntV    {} = IntT
typeOfValue DoubleV {} = DoubleT
typeOfValue StringV {} = StringT
typeOfValue VoidV   {} = VoidT
typeOfValue TagV    {} = TagT