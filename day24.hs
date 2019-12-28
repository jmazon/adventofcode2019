-- Day 24: Planet of Discord
{-# LANGUAGE FlexibleInstances #-}

import           Data.Array
import qualified Data.Set as S
import           Control.Monad (guard)

type Grid p = Array p Bool
class Ix p => HasNeighbors p where neighbors :: p -> [p]

main :: IO ()
main = do
  raw <- lines <$> getContents
  -- crude check that the input at least has the correct dimensions
  guard (length raw == 5)
  guard (all ((== 5) . length) raw)

  let grid = listArray euclidianBounds $ map (== '#') $ concat raw
  print $ biodiversity $ firstRepeat step grid

  -- My input was clear in the center.  Was everyone's?
  guard (not $ grid ! (0,0))
  let pGrid = accumArray (flip const) False plutonianBounds $
              map (\((i,j),e) -> ((0,i,j),e)) (assocs grid)
      pGrid' = iterate step pGrid !! 200
  print $ length $ filter (pGrid' !) $ indices pGrid'

type EPos = (Int,Int)
euclidianBounds :: (EPos,EPos)
euclidianBounds = ((-2,-2),(2,2))

instance HasNeighbors EPos where
  neighbors (i,j) = filter (inRange euclidianBounds)
                      [(i-1,j),(i,j+1),(i+1,j),(i,j-1)]

biodiversity :: Grid EPos -> Int
biodiversity = sum . zipWith (*) (iterate (*2) 1) . map fromEnum . elems

firstRepeat :: Ord a => (a -> a) -> a -> a
firstRepeat f = go S.empty where
  go cl s | s `S.member` cl = s
          | otherwise = go (S.insert s cl) (f s)

-- The only thing that distinguishes part 1 and part 2 is the
-- neighbors function.  So it's typeclassed out, and the code for the
-- game of life is shared.
step :: HasNeighbors p => Grid p -> Grid p
step grid = listArray (bounds grid) $ map cell (indices grid) where
  cell p | grid ! p  = neighborCount == 1
         | otherwise = neighborCount `elem` [1,2]
    where neighborCount = length $ filter (grid !) $ neighbors p

type PPos = (Int,Int,Int)
plutonianBounds :: (PPos,PPos)
plutonianBounds = ((-100,-2,-2),(100,2,2))

instance HasNeighbors PPos where
  -- I special-cased all levels' center cell to 0 neighbors so no bug
  -- would ever spawn here, and I didn't need to work around it
  -- everywhere else.  The base case is covered by an assertion in
  -- main.
  neighbors (_,0,0) = []
  neighbors p = filter (inRange plutonianBounds) $
                concatMap (plutonify p) [(0,-1),(-1,0),(0,1),(1,0)]
    where plutonify (d,i,j) (di,dj)
            | (_,0,0) <- n = [ (d+1,-2*di -x*signum dj,-2*dj -x*signum di)
                             | x <- [-2..2] ]
            | inRange plutonianBounds n = [n]
            | otherwise = [(d-1,di,dj)]
            where n = (d,i+di,j+dj)

-- Attempted, not worth it: implementing as a set of positions.
-- Pros: only the actual extent of infestation is stored.
-- Cons: the set management costs are too high compared to array.
