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

data Sheet cell = Sheet
  { _sheet_cells :: Map CellId cell
  }
  deriving (Show, Functor)


type CellValue = Either EvalError Domain

type SheetCells = Sheet Expr
type SheetValues = Sheet CellValue

makeLenses ''Expr
makeLenses ''Sheet
makePrisms ''EvalError
makePrisms ''CellId
