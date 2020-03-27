{-# LANGUAGE TemplateHaskell #-}
module Excel.Types where

import Internal.Imports
import qualified RIO.Map as Map

type Domain = Int
type CellExpr = Expr Domain

data Expr a where
  Lit :: a -> Expr a
  Ref :: CellId -> Expr Domain
  Apply :: Expr (a -> b) -> Expr a -> Expr b

data EvalError
  = EmptyCell
  | InvalidRef
  | CyclicReference
  deriving (Show, Eq)

newtype CellId = CellId Natural
  deriving (Show, Eq, Ord)

data Cell = Cell
  { _cell_id :: CellId
  , _cell_expr :: Expr Domain
  }

newtype Sheet cell = Sheet (Map CellId cell)
  deriving (Show, Eq, Functor)

type CellValue = Either EvalError Domain

type SheetCells = Sheet CellExpr
type SheetValues = Sheet CellValue

-- lenses
makeLenses ''Cell

-- isos
makePrisms ''CellId
makePrisms ''Sheet

-- prisms
makePrisms ''Expr
makePrisms ''EvalError
