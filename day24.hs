{-# LANGUAGE FlexibleInstances #-}
import qualified Data.Set as S
import Data.Array
import Control.Monad
import Control.Arrow

type Grid p = Array p Bool

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
              map (first ((,) 0)) (assocs grid)
      pGrid' = iterate step pGrid !! 200
  print $ length $ filter (pGrid' !) $ indices pGrid'

type EPos = (Int,Int)

biodiversity :: Grid EPos -> Int
biodiversity = sum . zipWith (*) (iterate (*2) 1) . map fromEnum . elems

firstRepeat :: Ord a => (a -> a) -> a -> a
firstRepeat f = go S.empty where
  go cl s | s `S.member` cl = s
          | otherwise = go (S.insert s cl) (f s)

euclidianBounds :: (EPos,EPos)
euclidianBounds = ((-2,-2),(2,2))

class Ix p => HasNeighbors p where neighbors :: p -> [p]
instance HasNeighbors EPos where
  neighbors (i,j) = filter (inRange euclidianBounds)
                      [(i-1,j),(i,j+1),(i+1,j),(i,j-1)]

step :: HasNeighbors p => Grid p -> Grid p
step grid = listArray (bounds grid) $ map cell (indices grid) where
  cell p | grid ! p  = neighborCount == 1
         | otherwise = neighborCount `elem` [1,2]
    where neighborCount = length $ filter (grid!) $ neighbors p

plutonianBounds :: (PPos,PPos)
plutonianBounds = ((-100,(-2,-2)),(100,(2,2)))

type PPos = (Int,EPos)
instance HasNeighbors PPos where neighbors = plutonianNeighbors

plutonianNeighbors :: PPos -> [PPos]
plutonianNeighbors (_,(0,0)) = []
plutonianNeighbors p = filter (inRange plutonianBounds) $
                       concatMap (neighbor p) [(0,-1),(-1,0),(0,1),(1,0)]
  where neighbor (d,(i,j)) (di,dj)
          | (_,(0,0)) <- n = [ (d+1,(-2*di -x*signum dj,-2*dj -x*signum di))
                             | x <- [-2..2] ]
          | inRange plutonianBounds n = [n]
          | otherwise = [(d-1,(di,dj))]
          where n = (d,(i+di,j+dj))

-- Attempted, not worth it: implementing as a set of positions.
-- Pros: only the actual extent of infestation is stored.
-- Cons: the set management costs are too high compared to array.
