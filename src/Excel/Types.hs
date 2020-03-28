{-# LANGUAGE TemplateHaskell #-}
module Excel.Types where

import Internal.Imports
import qualified RIO.Map as Map

type Domain = Int

data Expr where
  Lit :: Domain -> Expr
  Ref :: CellId -> Expr
  Add :: Expr -> Expr -> Expr
  Subtract :: Expr -> Expr -> Expr
  Multiply :: Expr -> Expr -> Expr
  Divide :: Expr -> Expr -> Expr
  Exponent :: Expr -> Expr -> Expr
  deriving (Show, Eq)

data EvalError
  = EmptyCell
  | InvalidRef
  | CyclicReference
  | DivideByZero
  | NegativeExponent
  deriving (Show, Eq)

newtype CellId = CellId Natural
  deriving (Show, Eq, Ord, Enum)

data Cell = Cell
  { _cell_id :: CellId
  , _cell_expr :: Expr
  }

data Write = Write
  { _write_cell :: CellId
  , _write_expr :: Expr
  }
  deriving (Show, Eq)

data Sheet cell = Sheet
  { _sheet_cells :: Map CellId cell
  , _sheet_maxId :: CellId
  }
  deriving (Show, Eq, Functor)

type CellValue = Either EvalError Domain

type SheetCells = Sheet Expr
type SheetValues = Sheet CellValue

makeLenses ''Expr
makeLenses ''Cell
makeLenses ''Sheet
makeLenses ''Write

makePrisms ''EvalError
makePrisms ''CellId
