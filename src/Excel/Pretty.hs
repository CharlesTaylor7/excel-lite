module Excel.Pretty where

import Internal.Imports
import Excel.Types

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty

class Pretty a where
  pretty :: a -> String

instance Pretty a => Pretty (Sheet a) where
  pretty = view $ _Sheet . folded . to pretty

instance Pretty Expr where
  pretty (Lit x) = pretty x
  pretty (Ref id) = pretty id

instance Pretty CellId where
  pretty = ('$':) . show . view _CellId

instance Pretty EvalError where
  pretty = show

instance Pretty Int where
  pretty = show

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = "[" <> joined <> "]"
    where joined = intercalate ", " [pretty a, pretty b, pretty c]

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = pretty ||| pretty

instance Pretty a => Pretty (Maybe a) where
  pretty = maybe "Nothing" pretty

instance Pretty a => Pretty [a] where
  pretty as = "(" <> join as <> ")"
    where join = intercalate ", " . map pretty
