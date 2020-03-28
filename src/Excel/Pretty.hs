module Excel.Pretty where

import Internal.Imports
import Excel.Types

import Text.Parsec (ParseError(..))


prettyPrint :: MonadIO m => Pretty a => a -> m ()
prettyPrint = liftIO . putStrLn . pretty

class Pretty a where
  pretty :: a -> String

instance Pretty ParseError where
  pretty = show

instance Pretty a => Pretty (Sheet a) where
  pretty sheet =
    let
      cellIds = enumFromTo (CellId 0) $ sheet ^. sheet_maxId
      getCell id = sheet ^? sheet_cells . ix id
      anyCells = has (sheet_cells . folded) sheet
    in
      if anyCells
      then
        cellIds
        & map (pretty . getCell)
        & intercalate " | "
      else "<empty sheet>"

instance Pretty Expr where
  pretty (Lit x) = pretty x
  pretty (Ref id) = pretty id
  pretty (Add a b) =
    parensP a <> "+" <> parensP b
  pretty (Subtract a b) =
    parensP a <> "-" <> parensP b
  pretty (Multiply a b) =
    parensP a <> "*" <> parensP b
  pretty (Divide a b) =
    parensP a <> "/" <> parensP b
  pretty (Exponent a b) =
    parensP a <> "^" <> parensP b

parens a = "(" <> a <> ")"
parensP = parens . pretty

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
  pretty = maybe " " pretty

instance Pretty a => Pretty [a] where
  pretty as = "(" <> join as <> ")"
    where join = intercalate ", " . map pretty
