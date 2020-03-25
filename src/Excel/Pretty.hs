module Excel.Pretty where

import Internal.Imports
import Excel.Types

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty

class Pretty a where
  pretty :: a -> String

instance Pretty Excel where

instance Pretty Expr where

instance Pretty Cell where
