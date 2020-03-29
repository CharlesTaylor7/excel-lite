module Main where

import Excel

main :: IO ()
main = evalStateT loop emptySheet

loop :: (MonadState SheetCells m, MonadIO m)
     => m ()
loop = do
  sheet <- get
  prettyPrint sheet
  input <- promptUser
  is_end <- runInput input
  case is_end of
    Nothing -> loop
    Just quit -> print quit

data End = UserQuit
  deriving Show
  
runInput :: (MonadState SheetCells m, MonadIO m)
         => Input
         -> m (Maybe End)
runInput = \case
    Eval expr -> do
      sheet <- get
      let val = evalExpr expr $ sheet
      prettyPrint val
      pure Nothing
    Assign assignment -> do
      modifySheet assignment
      pure Nothing
    Exec Quit -> pure . Just $ UserQuit

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
