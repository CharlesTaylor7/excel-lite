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
evalSheet sheet =
  flip execState emptySheet $
  flip runReaderT sheet $
  for_ (sheet ^. _Sheet . to Map.toList) $ uncurry evalNext

evalNext :: (MonadReader SheetCells m, MonadState SheetValues m)
         => CellId
         -> Expr
         -> m ()
evalNext id expr =
  let
    val = case expr of
      Lit num -> pure num
      Ref id ->
        view (_Sheet . at id) >>=
        maybe (throwError InvalidRef) (evalNext id)
  in do
    values <- get
    let cached = values ^? _Sheet . ix id
    let result = maybe val identity cached
    _Sheet . at id ?= result
-- eval :: (MonadReader SheetCells m, MonadError EvalError m)
--      => Expr
--      -> m Domain
-- eval (Lit num) = pure num
-- eval (Ref id) =
--   view (_Sheet . at id) >>=
--     maybe (throwError InvalidRef) eval
