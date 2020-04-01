{--
pseudo code (aka python )
def spirals():

  current = 1
  increment = 0
  yield 1
  for i in xrange(501):
    increment += 2
    for _ in xrange(4):
      current += increment
      yield current

def spiral_sum:
  sum = 0
  for x in spirals():
    sum += x
--}
{-# LANGUAGE NoOverloadedLists #-}
module Main where
import Control.Monad.ST
import Data.STRef
import Data.Foldable
import Prelude

main :: IO ()
main = print diagonalSpiralSum

modifyRead :: STRef s a -> (a -> a) -> ST s a
modifyRead ref f = modifySTRef' ref f >> readSTRef ref

diagonalSpiralSum :: Int
diagonalSpiralSum = runST $ do
  sumRef <- newSTRef 1
  incrementRef <- newSTRef 0
  currentRef <- newSTRef 1
  for_ [1..500] $ \_ -> do
    increment <- modifyRead incrementRef (+ 2)
    for_ [1..4] $ \_ -> do
      current <- modifyRead currentRef (+ increment)
      modifySTRef' sumRef (+ current)
  readSTRef sumRef
