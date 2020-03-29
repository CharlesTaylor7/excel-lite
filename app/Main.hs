module Main where

import Excel

main :: IO ()
main = evalStateT loop emptySheet

loop :: (MonadState SheetCells m, MonadIO m)
     => m ()
loop = do
  sheet <- get
  liftIO $ putStrLn "sheet: "
  prettyPrint sheet
  input <- promptUser
  runInput input
  loop

runInput :: (MonadState SheetCells m, MonadIO m)
         => Input
         -> m ()
runInput = \case
    Expr expr -> do
      sheet <- get
      let val = evalExpr expr $ sheet
      prettyPrint val
    Assign assignment ->
      modifySheet assignment

evalExpr :: Expr -> SheetCells -> CellValue
evalExpr expr sheet =
  eval expr
  & flip evalStateT emptySheet
  & flip runReader sheet

modifySheet :: MonadState SheetCells m => Assignment -> m ()
modifySheet (Assignment id expr) = modify $ setCell id expr

promptUser :: MonadIO m => m Input
promptUser = do
  liftIO $ putStrLn ">"
  line <- liftIO getLine
  case parseInput line of
    Left err -> prettyPrint err >> promptUser
    Right x -> pure x
