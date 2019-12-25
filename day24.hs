import qualified Data.Set as S
import Data.Array
import Control.Monad

type Pos = (Int,(Int,Int))
type Grid = Array Pos Bool

main :: IO ()
main = do
  raw <- lines <$> getContents
  guard (length raw == 5)
  guard (all ((== 5) . length) raw)

  let grid = listArray euclidianBounds $ map (== '#') $ concat raw
  print $ biodiversity $ firstRepeat (step euclidianNeighbors) grid

  guard (not $ grid ! (0,(0,0)))
  let pGrid = accumArray (flip const) False plutonianBounds (assocs grid)
      pGrid' = iterate (step plutonianNeighbors) pGrid !! 200
  print $ length $ filter (pGrid'!) $ indices pGrid'

biodiversity :: Grid -> Int
biodiversity = sum . zipWith (*) (iterate (*2) 1) . map fromEnum . elems

firstRepeat :: Ord a => (a -> a) -> a -> a
firstRepeat f = go S.empty where
  go cl s | s `S.member` cl = s
          | otherwise = go (S.insert s cl) (f s)

euclidianBounds :: (Pos,Pos)
euclidianBounds = ((0,(-2,-2)),(0,(2,2)))

euclidianNeighbors :: Pos -> [Pos]
euclidianNeighbors (d,(i,j)) = filter (inRange euclidianBounds)
  [(d,(i-1,j)),(d,(i,j+1)),(d,(i+1,j)),(d,(i,j-1))]

step :: (Pos -> [Pos]) -> Grid -> Grid
step neighbors grid = listArray (bounds grid) $ map cell (indices grid) where
  cell p | grid ! p  = neighborCount == 1
         | otherwise = neighborCount `elem` [1,2]
    where neighborCount = length $ filter (grid!) $ neighbors p

plutonianBounds :: (Pos,Pos)
plutonianBounds = ((-100,(-2,-2)),(100,(2,2)))

plutonianNeighbors :: Pos -> [Pos]
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
