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
  runExceptT $
  traverse_  (uncurry evalNext) $
    sheet ^.. _Sheet . ifolded . withIndex . to (over _1 Just)

runMaybe :: Maybe a -> b -> (a -> b) -> b
runMaybe may b f = maybe b f may

evalNext ::
         ( MonadReader SheetCells m
         , MonadState SheetValues m
         )
         => Maybe CellId
         -> Expr
         -> m CellValue
evalNext maybeId expr = do
  val <- runMaybe maybeId (pure Nothing) $
    \id -> use $ _Sheet . at id
  case val of
    Just x -> pure x
    Nothing -> do
      runMaybe maybeId (eval expr) $
        \id -> do
          _Sheet . at id ?= Left CyclicReference
          val <- eval expr
          _Sheet . at id ?= val
          pure val

eval ::
     ( MonadReader SheetCells m
     , MonadState SheetValues m
     )
     => Expr
     -> m CellValue
eval = \case
  Lit num -> pure . pure $ num
  Ref refId -> do
    cellExpr <- view $ _Sheet . at refId
    runMaybe cellExpr
      (pure $ Left InvalidRef)
      (evalNext (Just refId))
  Add expr1 expr2 -> runOp (+) expr1 expr2
  Multiply expr1 expr2 -> runOp (*) expr1 expr2
  where
    runOp op expr1 expr2 =
      let
        proc = ExceptT . evalNext Nothing
      in
        runExceptT $
          liftA2 op (proc expr1) (proc expr2)
