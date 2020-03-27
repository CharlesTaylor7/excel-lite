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
  traverse_ (uncurry evalNext) $
    sheet ^.. _Sheet . folding Map.toList . to (_1 %~ Just)

evalNext :: (MonadReader SheetCells m, MonadState SheetValues m)
         => Maybe CellId
         -> Expr
         -> m CellValue
evalNext maybeId expr = do
  val <- case maybeId of
    Just id -> use $ _Sheet . at id
    Nothing -> pure Nothing
  case val of
    Just x -> pure x
    Nothing -> do
      case maybeId of
        Just id ->  _Sheet . at id ?= Left CyclicReference
        Nothing -> pure ()
      val <- case expr of
        Lit num -> pure . pure $ num
        Ref refId -> do
          cell_expr <- view $ _Sheet . at refId
          case cell_expr of
            Nothing -> pure $ Left InvalidRef
            Just expr -> evalNext (Just refId) expr
        Apply expr1 expr2 -> do
          f <- evalNext Nothing expr1
          a <- evalNext Nothing expr2
          pure $ liftA2 ($) f a
      case maybeId of
        Just id -> _Sheet . at id ?= val
        Nothing -> pure ()

      pure val
