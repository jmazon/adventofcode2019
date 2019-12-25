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
plutonianNeighbors p = filter (inRange plutonianBounds) $ concat
  [leftNeighbor p,upNeighbor p,rightNeighbor p,downNeighbor p]
  where leftNeighbor (d,(_,-2)) = [(d-1,(0,-1))]
        leftNeighbor (d,(0,1)) = [(d+1,(i,2)) | i <- [-2..2]]
        leftNeighbor (d,(i,j)) = [(d,(i,j-1))]
        upNeighbor (d,(-2,_)) = [(d-1,(-1,0))]
        upNeighbor (d,(1,0)) = [(d+1,(2,j)) | j <- [-2..2]]
        upNeighbor (d,(i,j)) = [(d,(i-1,j))]
        rightNeighbor (d,(_,2)) = [(d-1,(0,1))]
        rightNeighbor (d,(0,-1)) = [(d+1,(i,-2)) | i <- [-2..2]]
        rightNeighbor (d,(i,j)) = [(d,(i,j+1))]
        downNeighbor (d,(2,_)) = [(d-1,(1,0))]
        downNeighbor (d,(-1,0)) = [(d+1,(-2,j)) | j <- [-2..2]]
        downNeighbor (d,(i,j)) = [(d,(i+1,j))]
