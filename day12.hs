-- Day 12: The N-Body Problem
{-# LANGUAGE FlexibleContexts,DeriveFunctor,DeriveFoldable #-}

import Text.Regex.PCRE
import Data.List           (delete,findIndices)
import Control.Arrow       ((&&&),(>>>))
import Control.Applicative (liftA2)
import System.Environment  (getArgs)
import System.IO

data V3 c = V3 { x :: !c, y :: !c, z :: !c }
            deriving (Show,Eq,Ord,Functor,Foldable)
instance Applicative V3 where pure e = V3 e e e
                              V3 f g h <*> V3 a b c = V3 (f a) (g b) (h c)
instance Num c => Num (V3 c) where (+) = liftA2 (+)
                                   (-) = liftA2 (-)
                                   (*) = undefined
                                   abs    = fmap abs
                                   signum = fmap signum
                                   fromInteger i = V3 (fromInteger i) 0 0

parseV3 :: String -> V3 Int
parseV3 l = V3 vx vy vz where
  [_,vx,vy,vz] = map read $ getAllTextSubmatches $
                 l =~ "<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>"

data Body v = Body { pos :: !v, vel :: !v } deriving (Show,Eq,Ord,Functor)
-- In my original implementation of part 1, Moon was specialized to
-- V3, not parameterized.  Hence not a Functor either.
type Moon1 = Body (V3 Int)

makeMoon :: Num a => a -> Body a
makeMoon p = Body p 0

step :: (Eq a,Num a) => [Body a] -> [Body a]
step moons = map stepMoon moons where
  stepMoon m@(Body p v) = Body (p+v') v' where
    v' = v + sum (map (signum . subtract p . pos) (delete m moons))

energy :: Moon1 -> (Int,Int)
energy = potential &&& kinetic where
  potential = sum . abs . pos
  kinetic   = sum . abs . vel
  
main :: IO ()
main = do
  steps <- read . head . (++ ["1000"]) <$> getArgs
  moons0 <- map (makeMoon . parseV3) . lines <$> getContents

  let moons' = iterate step moons0 !! steps
      energies = map (energy >>> id &&& uncurry (*)) moons'

  hPutStrLn stderr $ "After " ++ show steps ++ " steps:"
  mapM_ (hPrint stderr) moons'
  hPutStrLn stderr $ "Energy after " ++ show steps ++ " steps:"
  mapM_ (hPrint stderr) energies
  hPutStr stderr "Sum of total energy: "
  print $ sum $ map snd energies

  -- Brute force: find period for a set of moons
  -- putStrLn $ "Period: " ++ show (period moons)

  -- Smarter: we're in a non-euclidian (Manhattan) system where axes
  -- are independent.  [That's why we can usefully DeriveFunctor so
  -- easily.]  So we can find the period for a per-axis projected set
  -- of moons, and merge.
  let periods = map (\proj -> 2 * halfPeriod ((map . fmap) proj moons0)) [x,y,z]
  hPutStrLn stderr $ "Axis periods: " ++ show periods
  hPutStr stderr "Global period: "
  print $ foldr1 lcm periods

-- When I initially got my stars, I worked under the assumption that
-- “a” previous state would be “the” initial state.  And I was
-- concerned it could have been a lucky achievement.

-- That assumption seems all the more ridiculous now that I
-- spontaneously called my function “period” , even though it's
-- computed as time difference to the initial state.
-- All-the-more-lucky me!

-- Revisiting the question later on: it's actually not luck; “a” state
-- has to be “the” state.  It's provable.  What I failed to consider
-- back then is the importance of starting with immobile planets.

-- The N-body mechanics are *reversible*.  This stems from the fact
-- acceleration depends only on system position, and former system
-- position can be recovered simply by subtracting velocity.
--
-- So to reach a state again, we necessarily have to pass through its
-- predecessor state.
--
-- Suppose we've found a first state that matches exactly a previous
-- state.  Then that first state's predecessor is the same as its
-- matching previous state's predecesor.  But that would mean that
-- state is not the first to match a previous state, since its
-- predecessor qualifies too.  So we have a contradiction and that
-- state can't be the *first* repeat.  UNLESS we've found a state that
-- matches exactly a previous state with no predecessor.  QED

-- This covers unicity.  Existence has to be for the puzzle to make
-- sense (it *is* asking for one of its properties).  Does that state
-- exist by necessity, or only because of carefully chosen starting
-- conditions?
--
-- That's where the motionless initial state comes in.  Zero initial
-- velocity entails the repeat state's predecessor is very similar to
-- the state after a single step: it has the same positions and
-- opposite velocities.  So a period's ending states would have to be
-- mirrors of its starting states.  So from an arbitrary starting
-- position, we either come back or we don't.
--
-- Do we?  We do if and only if at some point velocity becomes
-- globally null again, because failing that we couldn't get on track
-- to mirror states back to the starting point.  [Still lacking the
-- non-hand-wavey argument for that happening.  My intuition says
-- starting static means no body can reach escape velocity, so both
-- the position and velocity spaces are bounded.]

-- Optimization 1: no need to store all the intermediate states, as
-- we'll only be comparing with the initial one.  This effectively
-- speeds things up by a factor of 5.

-- Optimization 2: we'll reach a reciprocal position where velocity is
-- null so we can come back.  So only track that; the period will be
-- its double.  Unsurprisingly, this halves solve time.

halfPeriod :: (Eq a,Num a) => [Body a] -> Int
halfPeriod st0 = opposite where
  (0:opposite:_) = findIndices (all ((== 0) . vel)) (iterate step st0)
