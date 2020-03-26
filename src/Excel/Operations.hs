module Excel.Operations
  ( emptySheet
  , emptyCell
  , setCell
  , readCell
  ) where

import Internal.Imports
import Excel.Types
import qualified RIO.Map as Map

emptySheet :: Excel
emptySheet = Excel Map.empty

emptyCell :: Cell
emptyCell = Cell
  { _cell_value = Left EmptyCell
  , _cell_expression = Nothing
  , _cell_dependents = []
  }

setCell :: CellId -> Expr -> Excel -> Excel
setCell id expr sheet =
  let
    pushDep sheet dep =
      sheet
      & _Excel . at dep %~
        Just .
        (cell_dependents %~ (id:)) .
        maybe emptyCell identity

    pushDeps sheet =
      foldl' pushDep sheet $ dependencies expr

    applyNewCell sheet =
      sheet
      & _Excel . at id ?~ (
        emptyCell
        & cell_expression ?~ expr
        & cell_value .~ eval expr sheet)
  in
    sheet
      & pushDeps
      & applyNewCell

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
eval (Ref id) excel =
  readCell id excel ^? _Right
  & maybe (Left InvalidRef) Right

dependencies :: Expr -> [CellId]
dependencies (Lit _) = []
dependencies (Ref id) = [id]
