-- Day 19: Tractor Beam
{-# LANGUAGE DeriveFunctor,DeriveFoldable #-}

import IntCode

type Pos = V2 Int
data V2 n = V !n !n deriving (Functor,Foldable)
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

-- This is still kind of dreadfully slow, but it does the job.  My
-- search algorithm is currently not too optimal, and my VM not that
-- fast.  First quick wins would be to clean up osearch, then to
-- perform a bit more prediction on the beam diameter.  VM
-- optimization independently.
main :: IO ()
main = do
  prg <- getIntCode
  let inBeam (V x y) = toEnum $ head $ evaluate prg [x,y]
      closeBeam = filter inBeam [ V x y | y <- [0..49], x <- [0..49] ]
      lastClose = last closeBeam
  print $ length closeBeam

  let osearch p z d = bsearch p z $ head $ filter (not . p . (z +))
                                  $ iterate (*2) d
      bsearch p z d = go z (z+d) where
        go a b | magnitude (b-a) <= 1 = a
               | p m                  = go m b
               | otherwise            = go a m
          where m = average a b
      -- limited to my case of Δx>Δy beams
      search z@(V x y) | w >= 100  = z
                       | otherwise = search $ osearch (not . inBeam) (V x (y+1))
                                                      (V 1 0)
                                            + V 1 0
        where w  = magnitude (z' - z) + 1
              z' = osearch inBeam z (V 1 (-1))
      V xt yt = search lastClose
  print $ 10000*xt +yt-99
