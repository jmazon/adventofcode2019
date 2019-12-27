-- Day 17: Set and Forget
{-# LANGUAGE DeriveFoldable #-}

import IntCode

import           Data.Array
import           Data.Char     (chr,ord)
import           Data.List     (group,sort)
import           Data.Maybe    (catMaybes)
import qualified Data.Vector

data Order = Forward | TurnLeft | TurnRight deriving (Eq,Show)
type Pos = V2 Int
data V2 n = V { x :: !n, y :: !n } deriving (Eq,Ord,Ix,Foldable)
instance Num n => Num (V2 n) where
  (V a b) + (V c d) = V (a+c) (b+d)
  (V a b) * (V c d) = V (a*c - b*d) (a*d + b*c)
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate (V a b) = V (-a) (-b)

main :: IO ()
main = do
  prg <- getIntCode
  let view = filter (not . null) $ lines $ map chr $ evaluate prg []
      width = length (head view)
      height = length view

  let grid = listArray (V 0 0,V (height-1) (width-1)) $ concat view
      [startPos] = filter ((`elem` "^v<>") . (grid !)) (indices grid)
      startDir | (grid!startPos) == '^' = V (-1) 0
      (_,path) = follow grid startPos startDir
      intersections = findIntersections path
  print $ sum $ map product intersections

  let outputs = evaluate (prg Data.Vector.// [(0,2)]) $ map ord solution
  print $ last outputs

follow :: Array Pos Char -> Pos -> Pos -> ([Order],[Pos])
follow grid pos0 dir0 = (orders,catMaybes path) where
  (orders,path) = unzip $ walk pos0 dir0
  walk pos dir
    | walkable pos'         = (Forward, Just pos) : walk pos' dir
    | walkable (pos + dirL) = (TurnLeft, Nothing) : walk pos  dirL
    | walkable (pos + dirR) = (TurnRight,Nothing) : walk pos  dirR
    | otherwise = []
    where pos' = pos + dir
          dirL = dir * i
          dirR = -dirL
          i = V 0 1
          walkable p = inRange (bounds grid) p && grid!p == '#'

-- My initial intersection detection algorithm simply checked for
-- plus-shaped patterns on the grid.  This works on AoC inputs, but
-- can theoretically be foiled by having e.g. three parallel unspaced
-- wires.
--
-- I wrote a wire-tracing algorithm for part2, and first tested it by
-- having it report intersections too.  I'm keeping the latter
-- intersection detection implementation, as the tracing has to be
-- done anyway, and gives (unobservably) better results.
findIntersections :: [Pos] -> [Pos]
findIntersections = map head. filter (not . null . tail) . group . sort

-- As most, I solved part 2 using my editor's search highlighting
-- feature.  Here's my solution, as a testament to my second star of
-- that day!
solution :: String
solution = unlines [ "B,A,B,A,C,A,C,B,C,C"
                   , "L,6,L,12,R,12,L,4"
                   , "R,12,L,10,L,10"
                   , "L,12,R,12,L,6"
                   , "n" ]
