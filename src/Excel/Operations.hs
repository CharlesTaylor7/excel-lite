module Excel.Operations where

import Internal.Imports
import Excel.Types
import qualified RIO.Map as Map

emptySheet :: Sheet a
emptySheet = Sheet Map.empty

setCell :: CellId -> Expr -> SheetCells -> SheetCells
setCell id expr = _Sheet . at id ?~ expr

readCell :: CellId -> SheetCells -> CellValue
readCell id sheet =
  let
    val = evalSheet sheet ^? _Sheet . ix id
  in
    join . toEither EmptyCell $ val
  where
    toEither :: a -> Maybe b -> Either a b
    toEither a = maybe (Left a) Right

evalSheet :: SheetCells -> SheetValues
evalSheet sheet = undefined
  -- flip execState emptySheet $
  -- flip runReaderT sheet $
  -- for_ (sheet ^. _Sheet . to Map.toList) $ uncurry evalNext

evalNext :: CellId -> Expr -> SheetCells -> SheetValues -> (SheetValues, CellValue)
evalNext id expr cells vals =
  let
    getVal :: CellId -> SheetValues -> Maybe CellValue
    getVal id = preview $ _Sheet . ix id

    val :: CellValue
    val = case expr of
      Lit num -> pure num
      Ref id -> do
        cell <- view $ _Sheet . at id
        cell & maybe (throwError InvalidRef) (evalNext id)
        getVal id

  in do
    values <- _
    let cached = values ^? _Sheet . ix id
    let result = maybe val identity cached
    _Sheet . at id ?= result
