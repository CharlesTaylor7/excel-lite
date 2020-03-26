module Excel.Operations where

import Internal.Imports
import Excel.Types
import qualified RIO.Map as Map

emptySheet :: Excel
emptySheet = Excel Map.empty

emptyCell :: Cell
emptyCell = Cell
  { _cell_value = Left EmptyCell
  , _cell_expression = Nothing
  , _cell_dependentCells = []
  }

readExpr :: String -> Maybe Expr
readExpr = undefined

setCell :: CellId -> Expr -> Excel -> Excel
setCell id expr excel =
  let
    val = eval expr excel
    cell = emptyCell
      & cell_expression ?~ expr
      & cell_value .~ val
  in
    excel & _Excel . at id ?~ cell

readCell :: CellId -> Excel -> Either EvalError Domain
readCell id excel =
  let
    val = excel ^? _Excel . ix id . cell_value
  in
    join . toEither EmptyCell $ val
  where
    toEither :: a -> Maybe b -> Either a b
    toEither a = maybe (Left a) Right

eval :: Expr -> Excel -> Either EvalError Domain
eval (Lit num) _ = pure num
