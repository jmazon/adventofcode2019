-- Day 10: Monitoring Station
{-# LANGUAGE DeriveFunctor,TypeApplications #-}

import Data.Ord
import Data.List
import Data.Function
import Control.Arrow

data V a = V { x :: !a, y :: !a } deriving (Eq,Functor)
newtype Angle = A { unA :: V Int } deriving Eq
instance Ord Angle where
  compare = comparing $ Down . (atan2 @Double <$> x <*> y) .
                        fmap fromIntegral . unA
-- There *was* a little bit of thought involved in inferring the
-- correct comparison formula to have it directly match the Elves'
-- laser rotation.

mapToCoords :: [String] -> [V Int]
mapToCoords = concat . zipWith (\i -> map (`V` i) . elemIndices '#') [0..]

dist :: V Int -> V Int -> Int
dist (V x1 y1) (V x2 y2) = abs (x2-x1) + abs (y2-y1)

angle :: V Int -> V Int -> Angle
angle (V x1 y1) (V x2 y2) = A $ V (dx `div` g) (dy `div` g) where
  dx = x2 - x1
  dy = y2 - y1
  g = max 1 $ gcd dx dy

nVisAngles :: [V Int] -> V Int -> Int
nVisAngles ps p0 = length $ group $ sort $ map (angle p0) $ delete p0 ps

main :: IO ()
main = do
  asteroids <- mapToCoords . lines <$> getContents

  let station = maximumBy (comparing $ nVisAngles asteroids) asteroids
  print $ nVisAngles asteroids station
  
  let lasered = delete station asteroids &
                map (angle station &&& id) &
                sortOn fst & groupBy ((==) `on` fst) &
                map (map snd >>> sortOn (dist station)) &
                transpose & concat
      (V bx by) = lasered !! 199
  print $ 100*bx + by
