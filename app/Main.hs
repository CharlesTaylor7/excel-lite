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
  write <- promptUser
  modifySheet write
  loop

modifySheet :: MonadState SheetCells m => Write -> m ()
modifySheet (Write id expr) = modify $ setCell id expr

promptUser :: MonadIO m => m Write
promptUser = do
  liftIO $ putStrLn ">"
  line <- liftIO getLine
  case parseInput line of
    Left err -> prettyPrint err >> promptUser
    Right x -> pure x
