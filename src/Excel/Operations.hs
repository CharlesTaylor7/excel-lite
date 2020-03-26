module Excel.Operations where

import Internal.Imports
import Excel.Types

newSheet :: Excel
newSheet = undefined

readExpr :: String -> Maybe Expr
readExpr = undefined

setCell :: CellId -> Expr -> Excel -> Excel
setCell = undefined

readCell :: CellId -> Excel -> Either EvalError Domain
readCell id excel =
  let
    val = excel ^? _Excel . ix id . cell_value
  in
    join . toEither NonexistentRef $ val
  where
    toEither :: a -> Maybe b -> Either a b
    toEither a = maybe (Left a) Right

eval :: Expr -> Excel -> Either EvalError Domain
eval (Lit num) _ = pure num
