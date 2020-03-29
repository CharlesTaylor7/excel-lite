{-# LANGUAGE FlexibleInstances #-}
module Excel.Pretty where

import Internal.Imports
import Excel.Types
import Excel.Operations

import Text.Parsec (ParseError(..))


prettyPrint :: MonadIO m => Pretty a => a -> m ()
prettyPrint = putStrLn . pretty

class Pretty a where
  pretty :: a -> String

instance Pretty ParseError where
  pretty = show

instance Pretty SheetCells where
  pretty sheet =
    let
      cellIds = enumFromTo (CellId 0) $ sheet ^. sheet_maxId
      values = evalSheet sheet
      getCell sheet id = sheet ^? sheet_cells . ix id
      prettify sheet = pretty . getCell sheet
      formatColumn = padToLongest . \id ->
        [ "$" <> show (id ^. _CellId)
        , prettify sheet id
        , prettify values id
        ]
    in
      cellIds
      & map formatColumn
      & transpose
      & map (surround "| " " |" . intercalate " | ")
      & intercalate "\n"

surround :: Semigroup a => a -> a -> a -> a
surround start end center = start <> center <> end

padToLongest :: [String] -> [String]
padToLongest texts =
  case maximumOf (folded . to length) texts of
    Just max -> map (padToCenter max) texts
    Nothing -> []

padToCenter :: Int -> String -> String
padToCenter m text = padding start <> text <> padding end
  where
    n = m - length text
    start = n `div` 2
    end = (n `div` 2) + (n `mod` 2)
    padding = flip replicate ' '

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

parens = surround "(" ")"
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

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = "(" <> joined <> ")"
    where joined = intercalate ", " [pretty a, pretty b]

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = pretty ||| pretty

instance Pretty a => Pretty (Maybe a) where
  pretty = maybe "" pretty

instance Pretty a => Pretty [a] where
  pretty as = parens $ join as
    where join = intercalate ", " . map pretty
