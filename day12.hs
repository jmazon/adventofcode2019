{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.List (delete,unfoldr)
import qualified Data.Set as S
import           Text.Regex.PCRE
import           Control.Arrow       ((&&&),(>>>))
import           Control.Applicative (liftA2)
import           Control.Exception   (assert)
import           System.Environment  (getArgs)
import           System.IO

data V3 c = V3 { x :: !c, y :: !c, z :: !c } deriving (Show,Eq,Ord,Functor)
instance Applicative V3 where pure e = V3 e e e
                              V3 f g h <*> V3 a b c = V3 (f a) (g b) (h c)
instance Num c => Num (V3 c) where (+) = liftA2 (+)
                                   (-) = liftA2 (-)
                                   (*) = undefined
                                   abs    = fmap abs
                                   signum = fmap signum
                                   fromInteger i = V3 (fromInteger i) 0 0

parseV3 l = V3 x y z where
  [_,x,y,z] = map read $ getAllTextSubmatches $
              l =~ "<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>"

data Body v = Body { pos :: !v, vel :: !v } deriving (Show,Eq,Ord,Functor)
-- In part1 Moon was specialized to V3, not parameterized.
-- Hence not a Functor either.

makeMoon p = Body p 0

step moons = map stepMoon moons where
  stepMoon m@(Body p v) = Body (p+v') v' where
    v' = v + sum (map (signum . subtract p . pos) (delete m moons))

energy = potential &&& kinetic where
  potential = innerSum . abs . pos
  kinetic   = innerSum . abs . vel
  innerSum (V3 a b c) = a + b + c
  
main = do
  steps <- read . head . (++ ["1000"]) <$> getArgs
  moons0 <- map (makeMoon . parseV3) . lines <$> getContents
  putStrLn $ "After " ++ show steps ++ " steps:"
  let moons' = iterate step moons0 !! steps
  mapM_ print moons'
  putStrLn $ "Energy after " ++ show steps ++ " steps:"
  let energies = map (energy >>> id &&& uncurry (*)) moons'
  mapM_ print energies
  putStrLn $ "Sum of total energy: " ++ show (sum (map snd energies))

  -- Brute force: find period for a set of moons
  -- putStrLn $ "Period: " ++ show (period moons)

  -- Smarter: find period for a per-axis projected set of moons
  let periods = map (\proj -> period (map (fmap proj) moons0)) [x,y,z]
  hSetBuffering stdout NoBuffering -- crude progress indicator
  putStrLn $ "Axis periods: " ++ show periods
  hSetBuffering stdout LineBuffering
  putStrLn $ "Global period: " ++ show (foldr1 lcm periods)

-- This period calculation scheme assumes “a” previous state is “the”
-- initial state.  It happens to work for this AoC, but isn't strict
-- in the question asked.  Maybe in a future day?
period st0 = length . unfoldr f $ (st0,S.empty) where
  f (st,s) | st `S.member` s = assert (st == st0) Nothing
           | otherwise = Just ((),(step st,st `S.insert` s))
