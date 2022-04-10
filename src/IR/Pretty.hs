module IR.Pretty where

import Prettyprinter

import IR.Syntax

----------------------------------------
-- Pretty printing instances
----------------------------------------

-- Binds

instance Pretty a => Pretty (Bind a) where
  pretty (LetB v e) = 
    pretty v <+> "=" <+> pretty e <> ";"
  pretty (RecB binds) = 
    align (vsep [ pretty (LetB v e) | (v, e) <- binds ])

-- Expressions

instance Pretty a => Pretty (Expr a) where
  pretty (VarE var) = 
    pretty var
  pretty (LitE lit) = 
    pretty lit
  pretty (ConE con) = 
    pretty con
  pretty (AppE fun arg) = 
    parens (pretty fun <+> pretty arg)
  pretty (LamE var exp) = 
    "\\" <> pretty var <+> "->" <+> pretty exp
  pretty (LetE bind exp) = 
    align (vsep ["let" <+> pretty bind, "in" <+> pretty exp])
  pretty (CaseE exp alts) = 
    align (vsep [ "case" <+> pretty exp <+> "of" <+> "{"
                , indent 4 (vsep (pretty <$> alts)) 
                , "}" ])

-- Alternatives

instance Pretty a => Pretty (Alt a) where
  pretty (LitA lit exp) = 
    pretty lit <+> "->" <+> pretty exp <> ";"
  pretty (ConA con vars exp) = 
    pretty con <+> hsep (pretty <$> vars) <+> "->" <+> pretty exp <> ";"
  pretty (DefA exp) = 
    "_" <+> "->" <+> pretty exp <> ";"
