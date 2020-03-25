module Excel.Types where

import Internal.Imports
import qualified RIO.Map as Map

data Expr where
  Lit :: Int -> Expr
  Ref :: CellId -> Expr
  Add :: Expr -> Expr -> Expr
  Multiply :: Expr -> Expr -> Expr

newtype CellId = CellId Natural
newtype CellValue = CellValue Int

data Cell = Cell
  { _cell_value :: CellValue
  , _cell_expression :: Expr
  , _cell_dependentCells:: [CellId]
  }

data Excel = Excel
  { _excel_cells :: Map CellId Cell
  }
