{-# LANGUAGE TypeApplications #-}

import qualified Data.Vector as V
import Data.Vector ((!),(//))
import Data.List.Split (linesBy)
import Data.List (find)

step v p = case v!p of
  1 -> (Just (p+4), v // [(v!(p+3),v!(v!(p+1)) + v!(v!(p+2)))])
  2 -> (Just (p+4), v // [(v!(p+3),v!(v!(p+1)) * v!(v!(p+2)))])
  99 -> (Nothing,v)

evaluate v arg1 arg2 = go (Just 0) (v // [(1,arg1),(2,arg2)]) where
  go Nothing v = v!0
  go (Just p) v = uncurry go (step v p)

main = do
  prg <- V.fromList . map (read @Int) . linesBy (== ',') <$> getContents
  print $ evaluate prg 12 2
  let Just (noun,verb) = find ((== 19690720) . uncurry (evaluate prg))
                         [ (i-j,j) | i <- [0..], j <- [0..i] ]
  print $ 100 * noun + verb
