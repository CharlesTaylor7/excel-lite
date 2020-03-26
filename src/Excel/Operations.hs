module Excel.Operations where

import Internal.Imports
import Excel.Types

newSheet :: Excel
newSheet = undefined

readExpr :: String -> Maybe Expr
readExpr = undefined

setCell :: CellId -> Expr -> Excel -> Excel
setCell = undefined

readCell :: CellId -> Excel -> Either EvalError Domain
readCell = undefined

eval :: Expr -> Excel -> Either EvalError Domain
eval (Lit num) _ = pure num
