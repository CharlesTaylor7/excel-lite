module Main where

import Excel

main :: IO ()
main =
  let
    Just expr1 = readExpr "2"
    Just expr2 = readExpr "2 * $0 + 2"
    excel = setCell (CellId 0) expr1 . setCell (CellId 1) expr2 $ newSheet
  in
    putStrLn . pretty $ excel
