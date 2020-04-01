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
import Data.IORef
import Data.Foldable
import Prelude

modifyRead :: IORef a -> (a -> a) -> IO a
modifyRead ref f = modifyIORef' ref f >> readIORef ref

main :: IO ()
main = do
  sumRef <- newIORef (1 :: Int)
  incrementRef <- newIORef 0
  currentRef <- newIORef 1
  for_ [1..500] $ \_ -> do
    increment <- modifyRead incrementRef (+ 2)
    for_ [1..4] $ \_ -> do
      current <- modifyRead currentRef (+ increment)
      modifyIORef' sumRef (+ current)

  sum <- readIORef sumRef
  print sum
{--
newtype Prev = Prev Int
newtype Inc = Inc Int

next :: Prev -> Inc -> [Int]
next (Prev p) (Inc i) = [p+i, p+2*i, p+3*i, p + 4*i]

--}
