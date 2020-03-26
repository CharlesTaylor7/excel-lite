module Excel.Operations where

import Internal.Imports
import Excel.Types
import qualified RIO.Map as Map

emptySheet :: Sheet
emptySheet = Sheet Map.empty $ CellId 0

newCell :: Cell
emptyCell = Cell
  { _cell_value = Left EmptyCell
  , _cell_expression = Nothing
  }

modifyCell :: (Cell -> a) -> Maybe Cell -> Maybe a
modifyCell f = Just . f . maybe emptyCell identity

setCell :: CellId -> Expr -> Sheet -> Sheet
setCell id expr sheet =
  let
    pushDep sheet dep =
      sheet
      & sheet_cells . at dep
      %~ modifyCell (cell_dependents %~ (id:))

    pushDeps sheet =
      foldl' pushDep sheet $ dependencies expr

    applyNewCell sheet =
      sheet
      & sheet_cells . at id
      %~ modifyCell (
        (cell_value .~ eval expr sheet) .
        (cell_expression ?~ expr)
      )
  in
    sheet
      & pushDeps
      & applyNewCell

readCell :: CellId -> Sheet -> Either EvalError Domain
readCell id excel =
  let
    val = excel ^? sheet_cells . ix id . cell_value
  in
    join . toEither EmptyCell $ val
  where
    toEither :: a -> Maybe b -> Either a b
    toEither a = maybe (Left a) Right

eval :: Expr -> Sheet -> Either EvalError Domain
eval (Lit num) _ = pure num
eval (Ref id) excel =
  readCell id excel ^? _Right
  & maybe (Left InvalidRef) Right

dependencies :: Expr -> [CellId]
dependencies (Lit _) = []
dependencies (Ref id) = [id]
