-- Day 6: Universal Orbit Map

import Data.Tree
import qualified Data.Map as M

readOrbit :: String -> (String,[String])
readOrbit l = (sun,[sat]) where (sun,_:sat) = break (== ')') l

main :: IO ()
main = do
  orbits <- map readOrbit . lines <$> getContents
  let objectSatellites = M.fromListWith (++) orbits
  let tree = unfoldTree (\s -> (s,M.findWithDefault [] s objectSatellites)) "COM"
  print $ sum $ zipWith (*) [0..] $ map length $ levels tree
  print $ hops tree "YOU" "SAN"

-- My pre-cleanup implementation for part 2 was an optimal in-order
-- traversal that maintained state of who had been spotted on the
-- orbit tree and short-circuited appropriately.  It ran fast.  It
-- handled the case where YOU and Santa could be in indirect orbit one
-- of another.  (more on that later)

-- I'm scrapping it for a simple filtered path expansion that's going
-- to be a lot easier to maintain.  Like AoC code actually needs to be
-- maintained.

hops :: Tree String -> String -> String -> Int
hops tree a b = length divergenceA + length divergenceB where
  (divergenceA,divergenceB) = stripCommon pathToA pathToB
  [pathToA] = foldTree (expand a) tree
  [pathToB] = foldTree (expand b) tree
  expand goal object paths | goal == object = [[]]
                           | otherwise      = map (object :) $ concat paths

stripCommon :: Eq a => [a] -> [a] -> ([a],[a])
stripCommon (a:as') (b:bs') | a == b = stripCommon as' bs'
stripCommon   as     bs              = (as,bs)

-- Smalltime bragging: I downloaded the statement and my puzzle input
-- before hopping on to an Internet-free part of a trip.  I correctly
-- guessed the second part and had it precomputed by the time
-- connectivity was restored.  The delay between my part 1 and 2 stars
-- is on account of a stupid off-by-two for being overwhelmed with joy
-- after my correct guess, and not reading the small print that the
-- we're couning hops between objects, not persons.
