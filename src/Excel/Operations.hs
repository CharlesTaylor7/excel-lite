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
  iforOf_ (_Sheet . ifolded) sheet $ evalNext . Just

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
  Subtract expr1 expr2 -> runOp (-) expr1 expr2
  Multiply expr1 expr2 -> runOp (*) expr1 expr2
  Divide expr1 expr2 -> runOpGuards div (expr1, noGuard) (expr2, divideByZeroGuard)
  Exponent expr1 expr2 -> runOpGuards (^) (expr1, noGuard) (expr2, negativeExponentGuard)
  where
    runOp op expr1 expr2 =
      runExceptT $ liftA2 op (proc expr1) (proc expr2)

    proc = ExceptT . evalNext Nothing

    runOpGuards op pair1 pair2 =
      runExceptT $ liftA2 op (proc' pair1) (proc' pair2)

    proc' (exp, guard) = apply guard . proc $ exp

data Guard f a = Guard
  { _guard_cond :: a -> Bool
  , _guard_act :: a -> f ()
  }

divideByZeroGuard :: MonadError EvalError m => Guard m Domain
divideByZeroGuard = Guard (== 0) (const $ throwError DivideByZero)

negativeExponentGuard :: MonadError EvalError m => Guard m Domain
negativeExponentGuard = Guard (< 0) (const $ throwError NegativeExponent)

noGuard :: Applicative m => Guard m a
noGuard = Guard (const False) (const $ pure ())

apply :: Monad m => Guard m a -> m a -> m a
apply (Guard cond act) m_a = do
  a <- m_a
  when (cond a) $ act a
  pure a
