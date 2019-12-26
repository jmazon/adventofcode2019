-- Day 11: Space Police

import IntCode

import           Data.Function ((&))
import qualified Data.Map as M
import           Control.Arrow ((***),(&&&),first)
import           Control.Monad (forM_,join)

data V2 = V { x :: !Int, y :: !Int } deriving (Eq,Ord)
instance Num V2 where
  V a b + V c d = V (a+c) (b+d)
  V a b * V c d = V (a*c - b*d) (b*c + a*d)
  negate (V a b) = V (negate a) (negate b)
  abs = undefined
  signum = undefined
  fromInteger n = V (fromInteger n) 0

i :: V2
i = V 0 1

paintHull :: RAM -> Bool -> M.Map V2 Bool
paintHull prg startColor = result where
  commandStream = pairs $ map toEnum $ evaluate prg $ map fromEnum cameraStream
  (cameraStream,~(result,_,_)) = lazyMapAccumL scan move
                                   (M.singleton 0 startColor,0,i) commandStream
  scan (kh,pos,_) = M.findWithDefault False pos kh -- “known hull”
  move (kh,pos,dir) (color,turn) = (M.insert pos color kh,pos+dir',dir')
    where dir' = dir * if turn then (-i) else i

main :: IO ()
main = do
  prg <- getIntCode
  let part1 = paintHull prg False
      part2 = paintHull prg True
      ((x1,x2),(y1,y2)) = M.keys part2 & map (x &&& y) &
                          unzip & minMax *** minMax
  print $ M.size part1
  forM_ [-y2 .. -y1] $ \sy -> do
    forM_ [x1..x2] $ \sx ->
      putChar $ if M.findWithDefault False (V sx (-sy)) part2 then '*' else ' '
    putChar '\n'

minMax :: Ord a => [a] -> (a,a)
minMax = foldr1 (\(a,b) (c,d) -> (min a c,max b d)) . join zip

pairs :: [a] -> [(a,a)]
pairs (a:b:xs) = (a,b) : pairs xs
pairs [] = []
pairs _ = error "pairs invoked on an odd-length list"

-- In this lazy variant of mapAccumL, the generating and accumulating
-- functions are separate, and the generating function only sees the
-- accumulator.  This way, that list element production can be
-- sequenced before input list element consumption.
lazyMapAccumL :: (a -> e') -> (a -> e -> a) -> a -> [e] -> ([e'],a)
lazyMapAccumL gen acc = go where
  go s es = first (gen s :) $ case es of e:es' -> go (acc s e) es'
                                         []    -> ([],s)
