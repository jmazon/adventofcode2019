-- Day 11: Space Police
{-# LANGUAGE RecordWildCards #-}

import IntCode (RAM,getIntCode,runAgent)

import Data.Function ((&))
import Data.Map.Strict hiding (map)
import Control.Arrow ((***),(&&&))
import Control.Monad (forM_,join)

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

data AgentState = AS { knownHull :: !(Map V2 Bool), pos :: !V2, dir :: !V2 }

paintHull :: RAM -> Bool -> Map V2 Bool
paintHull prg startColor =
  runAgent prg (Just . agent) knownHull (AS (singleton 0 startColor) 0 i)
  where agent AS {..} = ( findWithDefault False pos knownHull
                        , \color turn ->
                          let dir' = dir * if turn then (-i) else i
                          in AS (insert pos color knownHull) (pos+dir') dir' )

main :: IO ()
main = do
  prg <- getIntCode
  let painting1 = paintHull prg False
      painting2 = paintHull prg True
      ((x1,x2),(y1,y2)) = keys painting2 & map (x &&& y) &
                          unzip & minMax *** minMax
  print $ size painting1
  forM_ [-y2 .. -y1] $ \sy -> do
    forM_ [x1..x2] $ \sx ->
      putChar $ if findWithDefault False (V sx (-sy)) painting2 then '*' else ' '
    putChar '\n'

minMax :: Ord a => [a] -> (a,a)
minMax = foldr1 (\(a,b) (c,d) -> (min a c,max b d)) . join zip
