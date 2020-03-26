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
  deriving (Show)

data EvalError
  = EmptyCell
  | InvalidRef
  | CyclicReference
  deriving (Show, Eq)

newtype CellId = CellId Natural
  deriving (Show, Eq, Ord)

data Cell = Cell
  { _cell_value :: Either EvalError Domain
  , _cell_expression :: Maybe Expr
  , _cell_dependents:: [CellId]
  }
  deriving (Show)

data Sheet = Sheet
  { _sheet_cells :: Map CellId Cell
  , _sheet_maxId :: CellId
  }
  deriving (Show)

makeLenses ''Expr
makeLenses ''Cell
makeLenses ''Sheet
makePrisms ''EvalError
makePrisms ''CellId
