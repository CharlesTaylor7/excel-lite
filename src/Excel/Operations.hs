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
evalSheet sheet =
  flip execState emptySheet $
  flip runReaderT sheet $
  for_ (sheet ^. sheet_cells . to Map.toList) $ uncurry evalNext

evalNext :: (MonadReader SheetCells m, MonadState SheetValues m)
         => CellId
         -> Expr
         -> m ()
evalNext = _

eval :: (MonadReader SheetCells m, MonadError EvalError m)
     => Expr
     -> m Domain
eval (Lit num) = pure num
eval (Ref id) =
  view (sheet_cells . at id) >>=
    maybe (throwError InvalidRef) eval
