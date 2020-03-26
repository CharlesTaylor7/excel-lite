module Excel.Operations where

import Internal.Imports
import Excel.Types
import qualified RIO.Map as Map

emptySheet :: Sheet a
emptySheet = Sheet Map.empty

setCell :: CellId -> Expr -> SheetCells -> SheetCells
setCell id expr = sheet_cells . at id ?~ expr

readCell :: CellId -> SheetCells -> CellValue
readCell id sheet =
  let
    val = evalSheet sheet ^? sheet_cells . ix id
  in
    join . toEither EmptyCell $ val
  where
    toEither :: a -> Maybe b -> Either a b
    toEither a = maybe (Left a) Right

evalSheet :: SheetCells -> SheetValues
evalSheet sheet = fmap mapper sheet
  where
    mapper = flip runReaderT sheet . eval

eval :: (MonadReader SheetCells m, MonadError EvalError m)
     => Expr
     -> m Domain
eval (Lit num) = pure num
eval (Ref id) =
  view (sheet_cells . at id) >>=
    maybe (throwError InvalidRef) eval
