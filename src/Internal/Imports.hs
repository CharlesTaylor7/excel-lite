module Internal.Imports
  ( module Prelude
  , module Control.Arrow
  , module Control.Lens
  , module Data.Bifunctor
  , module Data.Monoid
  , module Data.List
  , module Data.List.NonEmpty
  , module Data.Ord
  , module Numeric.Natural
  , module RIO
  , module Control.Monad.State.Strict
  , module Control.Monad.Except
  , identity
  , print
  ) where

import Prelude (putStrLn, maximum, enumFromTo, getLine, read, repeat, zipWith)
import Data.Monoid
import Data.Ord
import Data.Bifunctor
import Data.List (intercalate, transpose, splitAt, sortOn)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Control.Arrow ((&&&), (|||))
import Data.Coerce(coerce)
import Numeric.Natural

-- rio
import RIO hiding (Lens, Lens', Getting, ASetter, ASetter', lens, (^.), to, view, over, set, sets, first, second, id)

-- lens
import Control.Lens

-- mtl
import Control.Monad.State.Strict
import Control.Monad.Except

identity :: a -> a
identity x = x

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . putStrLn . show
