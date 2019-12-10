import Data.Ord
import Data.List
import Data.Function
import Control.Arrow

mapToCoords = concat . zipWith (\y -> map (flip (,) y) . elemIndices '#') [0..]

dist (x0,y0) (x,y) = abs (x-x0) + abs (y-y0)

newtype Angle = A { unA :: (Int,Int) } deriving Eq
instance Ord Angle where
  compare = comparing $ Down . uncurry atan2 .
                        (fromIntegral *** fromIntegral) . unA

angle (x0,y0) (x,y) = (dx `div` g,dy `div` g) where
  dx = x - x0
  dy = y - y0
  g = max 1 $ gcd dx dy

nVisAngles ps p0 = length $ group $ sort $ map (angle p0) $ delete p0 ps

main = do
  asteroids <- mapToCoords . lines <$> getContents
  let station = maximumBy (comparing $ nVisAngles asteroids) asteroids
  putStrLn $ "Best is " ++ show station ++ " with " ++
             show (nVisAngles asteroids station) ++ " other asteroids detected."
  
  let lasered = delete station asteroids &
                map (A . angle station &&& id) &
                sortOn fst & groupBy ((==) `on` fst) &
                map (map snd >>> sortBy (comparing $ dist station)) &
                transpose & concat
      (x,y) = lasered !! 199
  print $ 100*x + y
