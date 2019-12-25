-- TODO: osearch between two best, not 0 and first

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
import IntCode

type Pos = (Int,Int)

main :: IO ()
main = do
  prg <- getIntCode
  let inBeam (x,y) = toEnum $ head $ evaluate prg [x,y]
      closeBeam = filter inBeam [ (x,y) | y <- [0..49], x <- [0..49] ]
      lastClose = last closeBeam
  print $ length closeBeam
  let osearch p z d = bsearch p z $ head $ filter (not . p . (z +)) $ iterate (*2) d
      bsearch p z d = go z (z+d) where
        go a b | magnitude (b-a) <= 1 = a
               | p m = go m b
               | otherwise = go a m
          where m = average a b
      -- limited to my case of dx>dy beams
      search z@(x,y) | w >= 100 = (x,y)
                     | otherwise = search (osearch (not . inBeam) (x,y+1) (1,0) + (1,0))
        where w = magnitude (z' - z) + 1
              z' = osearch inBeam z (1,-1)
      (xt,yt) = search lastClose
  print $ 10000*xt +yt-99

magnitude :: Pos -> Int
magnitude (x,y) = max (abs x) (abs y)

average :: Pos -> Pos -> Pos
average (a,b) (c,d) = ((a+c) `div` 2,(b+d) `div` 2)

instance Num (Int,Int) where
  (a,b) + (c,d) = (a+c,b+d)
  (a,b) * (c,d) = (a*c-b*d,b*c+a*d)
  (a,b) - (c,d) = (a-c,b-d)
  fromInteger i = (fromInteger i,0)
  abs (a,b) = (abs a,abs b)
  signum = undefined
