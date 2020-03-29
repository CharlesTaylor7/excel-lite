module Main where

import Excel

main :: IO ()
main = loop
  & flip evalStateT emptySheet
  & runExceptT
  >>= print ||| absurd

loop :: (MonadState SheetCells m, MonadIO m, MonadError End m)
     => m Void
loop = do
  sheet <- get
  prettyPrint sheet
  command <- promptUser
  runCommand command
  loop

data End = UserQuit
  deriving Show

runCommand :: (MonadState SheetCells m, MonadIO m, MonadError End m)
         => Command
         -> m ()
runCommand = \case
    Eval expr -> do
      sheet <- get
      let val = evalExpr expr $ sheet
      prettyPrint val
    Edit id -> putStrLn "Not Implemented"
    Delete id -> modify $ sheet_cells . at id .~ Nothing
    Quit -> throwError UserQuit

evalExpr :: Expr -> SheetCells -> CellValue
evalExpr expr sheet =
  eval expr
  & flip evalStateT emptySheet
  & flip runReader sheet

modifySheet :: MonadState SheetCells m => Assignment -> m ()
modifySheet (Assignment id expr) = modify $ setCell id expr

promptUser :: MonadIO m => m Command
promptUser = do
  putStrLn ">"
  line <- getLine
  case parseInput line of
    Left err -> prettyPrint err >> promptUser
    Right x -> pure x
