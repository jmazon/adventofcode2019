{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
import IntCode
import Data.Char
import Data.Array
import Debug.Trace
import Data.List
import Data.Maybe
import Control.Arrow
import Control.Monad
import qualified Data.Vector

main = do
  prg <- getIntCode
  let view = filter (not . null) $ lines $ map chr $ evaluate prg []
      width = length (head view)
      height = length view
      grid = listArray ((0,0),(height-1,width-1)) $ concat view
      [startPos] = filter ((`elem` "^v<>") . (grid !)) (indices grid)
      startDir | (grid!startPos) == '^' = (-1,0)
      intersections = findIntersections grid
      (orders,path) = unzip $ follow grid startPos startDir
      intersections2 = findIntersections2 path
      outputs = evaluate (prg Data.Vector.// [(0,2)]) $ map ord solution
  print $ sum $ map (uncurry (*)) intersections
  print $ sum $ map (uncurry (*)) intersections2
  putStr $ map chr $ init outputs
  print $ last outputs

a !? p | inRange (bounds a) p = Just (a!p)
       | otherwise = Nothing

isIntersection grid (i,j) = all ((== Just '#') . (grid !?))
                            [(i,j),(i-1,j),(i+1,j),(i,j-1),(i,j+1)]

findIntersections grid = filter (isIntersection grid) (indices grid)

data Order = Forward | TurnLeft | TurnRight deriving Eq

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

findIntersections2 = map head. filter (not . null . tail) . group . sort . catMaybes

rle = map compress . group where
  compress [TurnLeft] = "L"
  compress [TurnRight] = "R"
  compress f@(Forward:_) = show (length f)

factor xs = do
  let substringsL = map (head &&& uncurry (*) . (length &&& length . head)) . group . sort . filter (not . null) . concatMap inits . tails $ xs
      substrings = listArray (0,length substringsL-1) substringsL
      l = length xs
  a@(strA,nA) <- substringsL
  validateStr strA
  (strB,nB) <- delete a substringsL
  validateStr strB
  (strC,nC) <- bsearch substrings snd (l-nA-nB-1)
  validateStr strC
  guard (strC /= strA)
  guard (strC /= strB)
  f <- check [strA,strB,strC] (init xs)
  return (f,strA,strB,strC)

bsearch haystack pred needle = go 0 (snd (bounds haystack) + 1) where
  go a b | b-a <= 1 = mzero
         | otherwise = case compare (pred v) needle of
             LT -> go m b
             EQ -> pure v
             GT -> go a m
    where m = (a+b) `div` 2
          v = haystack!m

validateStr s = guard (length (intercalate "," s) <= 20)

check :: Eq a => [[a]] -> [a] -> [[Char]]
check ps = go where
  go [] = pure []
  go xs = do
    (xs',i) <- catMaybes $ zipWith (try xs) ps ['A'..]
    fmap (i :) (go xs')
  try :: Eq a => [a] -> [a] -> Char -> Maybe ([a],Char)
  try xs p i | Just xs' <- stripPrefix p xs = Just (xs',i)
             | otherwise = Nothing

solution = unlines [ "B,A,B,A,C,A,C,B,C,C"
                   , "L,6,L,12,R,12,L,4"
                   , "R,12,L,10,L,10"
                   , "L,12,R,12,L,6"
                   , "n" ]
