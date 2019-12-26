-- Day 11: Space Police
{-# LANGUAGE LambdaCase #-}

import IntCode

import           Data.Function ((&))
import           Data.Either   (partitionEithers)
import qualified Data.Map as M
import           Control.Arrow ((***),(&&&))
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
  commandStream = map toEnum $ evaluate prg $ map fromEnum cameraStream
  (cameraStream,~[result]) = partitionEithers $
                             go (M.singleton 0 startColor) 0 i commandStream
  -- {known hull} {current position} {current heading}
  go kh pos dir = (Left (M.findWithDefault False pos kh) :) .
    \case (color:turn:commands) ->
            let dir' = dir * if turn then (-i) else i
            in go (M.insert pos color kh) (pos+dir') dir' commands
          [] -> [Right kh]
          wtf -> error $ "Unexpected in command stream: " ++ show wtf

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
