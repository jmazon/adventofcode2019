-- Day 17: Set and Forget
{-# LANGUAGE FlexibleContexts,FlexibleInstances #-}

import IntCode

import           Data.Array
import           Data.Char     (chr,ord)
import           Data.List     (group,sort)
import           Data.Maybe    (catMaybes)
import qualified Data.Vector
import           Control.Monad (guard)

main :: IO ()
main = do
  prg <- getIntCode
  let view = filter (not . null) $ lines $ map chr $ evaluate prg []
      width = length (head view)
      height = length view
      grid = listArray ((0,0),(height-1,width-1)) $ concat view
      [startPos] = filter ((`elem` "^v<>") . (grid !)) (indices grid)
      startDir | (grid!startPos) == '^' = (-1,0)
      intersections = findIntersections grid
      (_,path) = unzip $ follow grid startPos startDir
      intersections2 = findIntersections2 path
      outputs = evaluate (prg Data.Vector.// [(0,2)]) $ map ord solution
  guard $ intersections == intersections2
  print $ sum $ map (uncurry (*)) intersections
  print $ last outputs

(!?) :: Array (Int,Int) a -> (Int,Int) -> Maybe a
a !? p | inRange (bounds a) p = Just (a!p)
       | otherwise = Nothing

isIntersection :: Array (Int,Int) Char -> (Int,Int) -> Bool
isIntersection grid (i,j) = all ((== Just '#') . (grid !?))
                            [(i,j),(i-1,j),(i+1,j),(i,j-1),(i,j+1)]

findIntersections :: Array (Int,Int) Char -> [(Int,Int)]
findIntersections grid = filter (isIntersection grid) (indices grid)

data Order = Forward | TurnLeft | TurnRight deriving (Eq,Show)

follow :: Array (Int,Int) Char -> (Int,Int) -> (Int,Int)
       -> [(Order, Maybe (Int,Int))]
follow grid pos dir
  | grid !? pos' == Just '#' = (Forward,Just pos) : follow grid pos' dir
  | grid !? (pos + dir * i) == Just '#' = (TurnLeft,Nothing) : follow grid pos (dir * i)
  | grid !? (pos + dir * (-i)) == Just '#' = (TurnRight,Nothing) : follow grid pos (dir * (-i))
  | otherwise = []
  where pos' = pos + dir
        i = (0,1)
  
instance Num (Int,Int) where
  (a,b) + (c,d) = (a+c,b+d)
  (a,b) * (c,d) = (a*c - b*d,a*d + b*c)
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate (a,b) = (-a,-b)

findIntersections2 ::[Maybe (Int,Int)] -> [(Int,Int)]
findIntersections2 = map head. filter (not . null . tail) . group . sort . catMaybes

-- As most, I solved part 2 using my editor's search highlighting
-- feature.  Here's my solution, as a testament to my second star of
-- the day!
solution :: String
solution = unlines [ "B,A,B,A,C,A,C,B,C,C"
                   , "L,6,L,12,R,12,L,4"
                   , "R,12,L,10,L,10"
                   , "L,12,R,12,L,6"
                   , "n" ]
