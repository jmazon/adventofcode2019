-- Day 17: Set and Forget
{-# LANGUAGE DeriveFoldable,LambdaCase,TupleSections #-}

import IntCode

import           Data.Array
import           Data.Char     (chr,ord)
import           Data.List     (group,sort,intercalate,intersperse,stripPrefix)
import           Data.Maybe    (catMaybes,mapMaybe)
import qualified Data.Vector
import           Control.Applicative ((<|>))
import           Text.Regex.PCRE

data Order = Forward | TurnLeft | TurnRight deriving (Eq,Show)
type Pos = V2 Int
type Dir = V2 Int
data V2 n = V !n !n deriving (Eq,Ord,Ix,Foldable)
instance Num n => Num (V2 n) where
  (V a b) + (V c d) = V (a+c) (b+d)
  (V a b) * (V c d) = V (a*c - b*d) (a*d + b*c)
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate (V a b) = V (-a) (-b)

findRobot :: Pos -> Char -> Maybe (Pos,Dir)
findRobot pos = fmap (pos,) . \case '^' -> Just $ V (-1)  0
                                    'v' -> Just $ V   1   0
                                    '<' -> Just $ V   0 (-1)
                                    '>' -> Just $ V   0   1
                                    _   -> Nothing

main :: IO ()
main = do
  prg <- getIntCode
  let view = filter (not . null) $ lines $ map chr $ evaluate prg []
      width = length (head view)
      height = length view

  let grid = listArray (V 0 0,V (height-1) (width-1)) $ concat view
      [(startPos,startDir)] = mapMaybe (uncurry findRobot) (assocs grid)
      (orders,path) = follow grid startPos startDir
      intersections = findIntersections path
  print $ sum $ map product intersections

  let solution = factor orders ++ ["n"]
  print $ last $ evaluate (prg Data.Vector.// [(0,2)]) $ map ord $ unlines solution

-- My initial intersection detection algorithm simply checked for
-- plus-shaped patterns on the grid.  This works on AoC inputs, but
-- can theoretically be foiled by having e.g. three parallel unspaced
-- wires.
--
-- I wrote a wire-tracing algorithm for part 2, and first tested it by
-- having it report intersections too.  I'm keeping the latter
-- intersection detection implementation, as the tracing has to be
-- done anyway, and gives (unobservably) better results.
findIntersections :: [Pos] -> [Pos]
findIntersections = map head. filter (not . null . tail) . group . sort

follow :: Array Pos Char -> Pos -> Dir -> (String,[Pos])
follow grid pos0 dir0 = (intercalate "," (rle orders 1),catMaybes path) where
  (orders,path) = unzip $ walk pos0 dir0
  walk pos dir
    | walkable pos'         = (Forward, Just pos) : walk pos' dir
    | walkable (pos + dirL) = (TurnLeft, Nothing) : walk pos  dirL
    | walkable (pos + dirR) = (TurnRight,Nothing) : walk pos  dirR
    | otherwise = []
    where pos' = pos + dir
          dirL = dir * V 0 1
          dirR = -dirL
          walkable p = inRange (bounds grid) p && grid!p == '#'
  rle [] _ = []
  rle (o:os) d = case o of Forward | (Forward:_) <- os -> rle os $! d+1
                                   | otherwise         -> show d : rest
                           TurnLeft  -> "L" : rest
                           TurnRight -> "R" : rest
    where rest = rle os (1 :: Int)

-- As did most, I solved part 2 using my editor's search highlighting
-- feature.  I'm rewriting for more genericity.  That kind of search
-- on a small space is well suited to PCRE's backtracking abilities.
-- Ironically, even though it makes finding the movement functions
-- very easy, mapping them back to the main movement routine isn't as
-- feasible: PCRE (will match but) won't capture (any but the last in)
-- a repeated group.  So I'm back to a greedy search, which likely
-- works fine on AoC inputs, but could fail if a function was a prefix
-- of another.
--
-- It's not really worth upgrading until I actually go the extra mile:
--   • backtrack from a match that gives correct functions but too
--     long a main routine
--   • up the ante and find solutions that stray off the single path
factor :: String -> [String]
factor orders = intersperse ',' routine : map tail functions where
  delimited = ',' : orders
  _:functions = getAllTextSubmatches $ delimited =~
    "^(,.{1,20})\\1*(,.{1,20})(?:\\1|\\2)*(,.{1,20})(?:\\1|\\2|\\3)*$"
  Just routine = munch delimited
  munch [] = pure []
  munch s = do (f,s') <- foldr1 (<|>) $
                         zipWith (\l f -> fmap (l,) (stripPrefix f s))
                           "ABC" functions
               (f :) <$> munch s'
