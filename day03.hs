-- Day 3: Crossed Wires

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import           Data.Map.Strict ((!))
import qualified Data.Set as S

data V2 = V !Int !Int deriving (Eq,Ord)
data Direction = R | L | U | D deriving Read
data PathStep = PathStep !Direction !Int
type WireStep = (V2,Int)

readPath :: String -> [PathStep]
readPath = map toStep . wordsBy (== ',') where
  toStep (d:l) = PathStep (read [d]) (read l)
  toStep  wtf  =  error $ "Couldn't parse path step: " ++ wtf

directionDelta :: Direction -> V2
directionDelta R = V   0   1
directionDelta L = V   0 (-1)
directionDelta U = V (-1)  0
directionDelta D = V   1   0

wirePath :: [PathStep] -> [WireStep]
wirePath = concat . snd . mapAccumL adv (V 0 0,0) where
  adv (V x y,a) (PathStep d l) = ( (V (x+l*dx) (y+l*dy),a+l)
                                 , [ (V (x+i*dx) (y+i*dy),a+i) | i <- [1..l] ])
    where V dx dy = directionDelta d

dist :: V2 -> Int
dist (V x y) = abs x + abs y

main :: IO ()
main = do
  w1 <- M.fromListWith min . wirePath . readPath <$> getLine
  w2 <- M.fromListWith min . wirePath . readPath <$> getLine
  let intersections = S.elems $ S.intersection (M.keysSet w1) (M.keysSet w2)
  print $ minimum $ map dist intersections
  print $ minimum $ map (\i -> w1!i + w2!i) intersections
