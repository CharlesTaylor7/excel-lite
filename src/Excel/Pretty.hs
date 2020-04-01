{-# LANGUAGE FlexibleInstances #-}
module Excel.Pretty where

import Internal.Imports
import Excel.Types
import Excel.Operations

import Text.Parsec (ParseError(..))
import Data.Functor.Foldable

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
  pretty = cata prettyF

prettyF :: ExprF String -> String
prettyF = \case
  LitF x -> pretty x
  RefF id -> pretty id
  AddF a b -> parens a <> " + " <> parens b
  SubtractF a b -> parens a <> " - " <> parens b
  MultiplyF a b -> parens a <> " * " <> parens b
  DivideF a b -> parens a <> " / " <> parens b
  ExponentF a b -> parens a <> " ^ " <> parens b

parens :: String -> String
parens = surround "(" ")"

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
  pretty = brackets . cata (joinF ", ") . map pretty

joinF :: Monoid a => a -> ListF a a -> a
joinF separator = \case
  Nil -> mempty
  Cons a b -> a <> separator <> b

brackets :: String -> String
brackets = surround "[" "]"
