module Excel.Operations where

import Internal.Imports
import Excel.Types
import qualified RIO.Map as Map

emptySheet :: SheetCells
emptySheet = Sheet Map.empty

setCell :: CellId -> Expr -> SheetCells -> SheetCells
setCell id expr = sheet_cells . at id ?~ expr

readCell :: CellId -> SheetCells -> Either EvalError Domain
readCell id sheet =
  let
    val = sheet ^? sheet_cells . ix id . to (flip eval sheet)
  in
    join . toEither EmptyCell $ val
  where
    toEither :: a -> Maybe b -> Either a b
    toEither a = maybe (Left a) Right

eval :: Expr -> SheetCells -> Either EvalError Domain
eval (Lit num) _ = pure num
eval (Ref id) sheet =
  readCell id sheet ^? _Right
  & maybe (Left InvalidRef) Right

evalSheet :: SheetCells -> SheetValues
evalSheet sheet = fmap (flip eval sheet) sheet

dependencies :: Expr -> [CellId]
dependencies (Lit _) = []
dependencies (Ref id) = [id]
