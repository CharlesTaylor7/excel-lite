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

newtype JoinItem = JoinItem String

instance Semigroup JoinItem where
  JoinItem "" <> x = x
  x <> JoinItem "" = x
  JoinItem x <> JoinItem y = JoinItem $ x <> " | " <> y

instance Monoid JoinItem where
  mempty = JoinItem ""

instance Pretty JoinItem where
  pretty (JoinItem "") = "<empty sheet>"
  pretty (JoinItem x) = x

instance Pretty a => Pretty (Sheet a) where
  pretty sheet =
    let
      joinPretty = JoinItem . pretty
      cellIds = enumFromTo (CellId 0) $ sheet ^. sheet_maxId
      joined = cellIds ^. folded . to joinPretty
    in
      pretty joined

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
  pretty = maybe "Nothing" pretty

instance Pretty a => Pretty [a] where
  pretty as = "(" <> join as <> ")"
    where join = intercalate ", " . map pretty
