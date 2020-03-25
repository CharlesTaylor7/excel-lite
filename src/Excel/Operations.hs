module Excel.Operations where

import Internal.Imports
import Excel.Types

newSheet :: Excel
newSheet = undefined

readExpr :: String -> Maybe Expr
readExpr = undefined

setCell :: CellId -> Expr -> Excel -> Excel
setCell = undefined

readCell :: Excel -> CellId -> CellValue
readCell = undefined
