import Data.Array
import qualified Data.Set as S
import Control.Arrow
import Control.Monad

main :: IO ()
main = do
  raw <- lines <$> getContents
  guard (length raw == 5)
  guard (all ((== 5) . length) raw)
  let grid = listArray ((-2,-2),(2,2)) $ map (== '#') $ concat raw
  print $ biodiversity $ firstRepeat step grid

  let gridd = accumArray (flip const) False ((-200,(-2,-2)),(200,(2,2))) $
              map (first ((,) 0)) (assocs grid)
  let gridd' = iterate stepp gridd !! 200
  print $ length $ filter (gridd'!) $ filter ((/= (0,0)) . snd) $ indices gridd'

biodiversity :: Array (Int,Int) Bool -> Int
biodiversity = sum . zipWith (*) (iterate (*2) 1) . map fromEnum . elems

firstRepeat :: Ord a => (a -> a) -> a -> a
firstRepeat f = go S.empty where
  go cl s | s `S.member` cl = s
          | otherwise = go (S.insert s cl) (f s)

step :: Array (Int,Int) Bool -> Array (Int,Int) Bool
step grid = listArray (bounds grid) $ map cell (indices grid) where
  cell p | grid!p = neighborCount == 1
         | otherwise = neighborCount == 1 || neighborCount == 2
    where neighborCount = length $ filter (grid!) $ euclidianNeighbors p

type EPos = (Int,Int)
type PPos = (Int,EPos)

layoutBounds :: (EPos,EPos)
layoutBounds = ((-2,-2),(2,2))

euclidianNeighbors :: EPos -> [EPos]
euclidianNeighbors (i,j) = filter (inRange layoutBounds)
  [(i-1,j),(i,j+1),(i+1,j),(i,j-1)]

stepp :: Array (Int,(Int,Int)) Bool -> Array (Int,(Int,Int)) Bool
stepp gridd = listArray (bounds gridd) $ map cell (indices gridd) where
  cell p | (d,(0,0)) <- p = error ("Evaled at level " ++ show d)
         | gridd!p = neighborCount == 1
         | otherwise = neighborCount == 1 || neighborCount == 2
    where neighborCount | abs (fst p) == 200 = 0
                        | otherwise = length $ filter (gridd!) $ plutonianNeighbors p

plutonianNeighbors :: PPos -> [PPos]
plutonianNeighbors p = concat
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
