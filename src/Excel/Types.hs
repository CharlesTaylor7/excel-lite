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
  = CyclicReference
  | InvalidRef
  | NonexistentRef
  deriving (Eq, Show)

newtype CellId = CellId Natural

data Cell = Cell
  { _cell_value :: Either EvalError Domain
  , _cell_expression :: Expr
  , _cell_dependentCells:: [CellId]
  }

data Excel = Excel
  { _excel_cells :: Map CellId Cell
  }

makeLenses ''Expr
makeLenses ''Cell
makeLenses ''Excel
makePrisms ''EvalError
makePrisms ''CellId
