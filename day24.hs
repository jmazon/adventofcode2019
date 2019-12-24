import Data.Array
import qualified Data.Set as S
import Control.Arrow
import Control.Monad

main :: IO ()
main = do
  raw <- lines <$> getContents
  guard (length raw == 5)
  guard (all ((== 5) . length) raw)
  let grid = listArray ((1,1),(5,5)) $ map (== '#') $ concat raw
  print $ biodiversity $ firstRepeat step grid

  let gridd = accumArray (flip const) False ((-200,(1,1)),(200,(5,5))) $
              map (first ((,) 0)) (assocs grid)
  let gridd' = iterate stepp gridd !! 200
  print $ length $ filter (gridd'!) $ filter ((/= (3,3)) . snd) $ indices gridd'

biodiversity :: Array (Int,Int) Bool -> Int
biodiversity = sum . zipWith (*) (iterate (*2) 1) . map fromEnum . elems

firstRepeat :: Ord a => (a -> a) -> a -> a
firstRepeat f = go S.empty where
  go cl s | s `S.member` cl = s
          | otherwise = go (S.insert s cl) (f s)

step :: Array (Int,Int) Bool -> Array (Int,Int) Bool
step grid = listArray (bounds grid) $ map cell (indices grid) where
  cell p@(i,j) | grid!p = neighborCount == 1
               | otherwise = neighborCount == 1 || neighborCount == 2
    where neighborCount = length $ filter (grid!) $ filter (inRange (bounds grid)) [(i-1,j),(i,j+1),(i+1,j),(i,j-1)]

stepp :: Array (Int,(Int,Int)) Bool -> Array (Int,(Int,Int)) Bool
stepp gridd = listArray (bounds gridd) $ map cell (indices gridd) where
  cell p | (d,(3,3)) <- p = error ("Evaled at level " ++ show d)
         | gridd!p = neighborCount == 1
         | otherwise = neighborCount == 1 || neighborCount == 2
    where neighborCount | abs (fst p) == 200 = 0
                        | otherwise = length $ filter (gridd!) $ concat [leftNeighbor p,upNeighbor p,rightNeighbor p,downNeighbor p]
          leftNeighbor (d,(_,1)) = [(d-1,(3,2))]
          leftNeighbor (d,(3,4)) = [(d+1,(i,5)) | i <- [1..5]]
          leftNeighbor (d,(i,j)) = [(d,(i,j-1))]
          upNeighbor (d,(1,_)) = [(d-1,(2,3))]
          upNeighbor (d,(4,3)) = [(d+1,(5,j)) | j <- [1..5]]
          upNeighbor (d,(i,j)) = [(d,(i-1,j))]
          rightNeighbor (d,(_,5)) = [(d-1,(3,4))]
          rightNeighbor (d,(3,2)) = [(d+1,(i,1)) | i <- [1..5]]
          rightNeighbor (d,(i,j)) = [(d,(i,j+1))]
          downNeighbor (d,(5,_)) = [(d-1,(4,3))]
          downNeighbor (d,(2,3)) = [(d+1,(1,j)) | j <- [1..5]]
          downNeighbor (d,(i,j)) = [(d,(i+1,j))]
