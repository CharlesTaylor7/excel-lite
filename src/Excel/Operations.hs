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

evalNext :: (MonadReader SheetCells m, MonadState SheetValues m)
         => CellId
         -> Expr
         -> m CellValue
evalNext id expr = do
  val <- use $ _Sheet . at id
  case val of
    Just x -> pure x
    Nothing -> do
      val <- case expr of
        Lit num -> pure . pure $ num
        Ref id -> do
          cell_expr <- view $ _Sheet . at id
          case cell_expr of
            Nothing -> pure $ Left InvalidRef
            Just expr -> evalNext id expr
      _Sheet . at id ?= val
      pure val
          -- let foo = maybe (Left InvalidRef) (_ . evalNext id) cell
          -- pure foo

  --   val :: m CellValue
  --   val = case expr of
  --     Lit num -> pure . pure $ num
  --     Ref id -> do
  --       cell <- view $ _Sheet . at id
  --       cell & maybe (throwError InvalidRef) (evalNext id)
  --       val <- getVal id
  --       undefined
  --       -- maybe (throwError EmptyCell) pure val
  -- in do
  --   values <- get
  --   let cached = values ^? _Sheet . ix id
  --   let result = maybe val identity cached
  --   _Sheet . at id <?= result
