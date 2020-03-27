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
  traverse_ (runExceptT . uncurry evalNext) $
    sheet ^.. _Sheet . folding Map.toList . to (_1 %~ Just)

runMaybe :: Maybe a -> b -> (a -> b) -> b
runMaybe may b f = maybe b f may

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust may f = maybe (pure ()) f may

evalNext ::
         ( MonadReader SheetCells m
         , MonadState SheetValues m
         )
         => Maybe CellId
         -> Expr
         -> m Domain
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
     , MonadError EvalError m
     )
     => Expr
     -> m Domain
eval = \case
  Lit num -> pure num
  Ref refId -> do
    cellExpr <- view $ _Sheet . at refId
    runMaybe cellExpr
      (pure $ Left InvalidRef)
      (evalNext (Just refId))
  Add expr1 expr2 -> do
    val1 <- _evalNext Nothing expr1
    val2 <- _evalNext Nothing expr2
    pure $ liftA2 (+) val1 val2
  Multiply expr1 expr2 -> do
    val1 <- _evalNext Nothing expr1
    val2 <- _evalNext Nothing expr2
    pure $ liftA2 (*) val1 val2
