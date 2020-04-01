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
  = InvalidRef
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

data Sheet cell = Sheet
  { _sheet_cells :: Map CellId cell
  , _sheet_maxId :: CellId
  }
  deriving (Show, Eq, Functor)

data Command
  = Eval Expr
  | Quit
  | Edit CellId
  | Delete CellId
  deriving (Show, Eq)

type CellValue = Either EvalError Domain

type SheetInput = Sheet Text
type SheetCells = Sheet Expr
type SheetValues = Sheet CellValue

-- isos
makePrisms ''CellId

-- lenses
makeLenses ''Cell
makeLenses ''Sheet

-- prisms
makePrisms ''Expr
makePrisms ''EvalError
makePrisms ''Command
