-- Day 19: Tractor Beam
{-# LANGUAGE DeriveFunctor,DeriveFoldable,NamedFieldPuns #-}

import IntCode (getIntCode,runIntStream)
import Data.Ord
import Data.List
import Numeric.Search

type Pos = V2 Int
data V2 n = V { x :: !n, y :: !n } deriving (Functor,Foldable,Show)
instance Num n => Num (V2 n) where
  V a b + V c d = V (a+c) (b+d)
  V a b * V c d = V (a*c-b*d) (b*c+a*d)
  V a b - V c d = V (a-c) (b-d)
  fromInteger i = V (fromInteger i) 0
  abs = fmap abs
  signum = undefined

magnitude :: Pos -> Int
magnitude = maximum . abs

average :: Pos -> Pos -> Pos
average z1 z2 = fmap (`div` 2) (z1 + z2)

main :: IO ()
main = do
  -- Part 1: implement exactly as asked.
  prg <- getIntCode
  let inBeam V{x,y} = toEnum o where [o] = runIntStream prg [x,y]
      closeBeam = filter inBeam [ V x y | y <- [0..49], x <- [0..49] ]
  print $ length closeBeam

  -- Part 2: I got my star with a linear scan by increasing y (my
  -- puzzle input has Δx>Δy), nested linear scan on x to keep on the
  -- beam's bottom edge, and flaky open search to compute the width.
  -- Really slow, especially with my way-too-purely functional
  -- copy-the-entire-RAM-at-each-uodate VM.

  -- This updated version is much better and more general (works for
  -- other beam directions): compute beam slope—an approximation is
  -- enough, as we know is starts on 0 and only widens with
  -- distance—and find optimal location using exponential search.
  -- With a slight catch: with bad aliasing luck, the rasterized
  -- projection of the beam could contract and widen at every step.
  -- But this can only occur on different x+y sum parities, so by
  -- searching both parities separately, we can keep great search
  -- speed with no risk of missing the spot.

  -- Approximate middle of beam by midpoint of boundaries of beam
  -- with the 50x50 origin box.
  let closeBoundary = filter (\V{x,y} -> x == 49 || y == 49) closeBeam
      V xl yl = minimumBy (comparing y <> comparing (Down . x)) closeBoundary
      V xh yh = maximumBy (comparing y <> comparing (Down . x)) closeBoundary
      (slX,slY) = ((xl + xh) `div` 2,(yl + yh) `div` 2)

  -- Given point in beam, compute constant-x-y-sum beam width
  let widthAround z = ( left + right + 1
                      , (crossBeamAt left,crossBeamAt (-right)) ) where
        left = halfWidthAround id
        right = halfWidthAround negate
        halfWidthAround f = w where
          Just w = largest True $ search positiveExponential divForever $
                   \n -> inBeam (crossBeamAt (f n))
        crossBeamAt n = z + V (-n) n

  -- Given x+y sum, compute closest point to (approximate) middle of beam
  let beamAt s = V ((s * slX + slS `div` 2) `div` slS)
                   ((s * slY + slS `div` 2) `div` slS)
      slS = slX + slY

  -- Beam width is increasing by sum(x+y) *and parity*.
  -- So compute minimum for both parities and keep minimum.
  let closestWith f = f a where
        Just a = smallest True $ search positiveExponential divForever $
                 \n -> fst (widthAround (beamAt (f n))) >= 100
      closestP1 = closestWith (\n -> slS + 2*n)
      closestP2 = closestWith (\n -> slS + 2*n + 1)
      closest = min closestP1 closestP2
      (_,(l,r)) = widthAround $ beamAt closest
  print $ 10000 * x l + y r
