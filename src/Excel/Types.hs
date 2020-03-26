{-# LANGUAGE TemplateHaskell #-}
module Excel.Types where

import Internal.Imports
import qualified RIO.Map as Map

type Domain = Int

data Expr where
  Lit :: Domain -> Expr
  Ref :: CellId -> Expr
  Add :: Expr -> Expr -> Expr
  Multiply :: Expr -> Expr -> Expr

data EvalError
  = EmptyCell
  | InvalidRef
  | CyclicReference
  deriving (Eq, Show)

newtype CellId = CellId Natural
  deriving (Eq, Ord)

data Cell = Cell
  { _cell_value :: Either EvalError Domain
  , _cell_expression :: Maybe Expr
  , _cell_dependentCells:: [CellId]
  }

newtype Excel = Excel
  { _excel_cells :: Map CellId Cell
  }

makeLenses ''Expr
makeLenses ''Cell
makePrisms ''Excel
makePrisms ''EvalError
makePrisms ''CellId
