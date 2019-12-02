import qualified Data.Vector as V
import Data.Vector ((!),(//))
import Data.List.Split (linesBy)
import Data.List (find)

evaluate v arg1 arg2 = go (v // [(1,arg1),(2,arg2)]) 0 where
  go v p = case v!p of
    1 -> go (v // [(v!(p+3),v!(v!(p+1)) + v!(v!(p+2)))]) (p+4)
    2 -> go (v // [(v!(p+3),v!(v!(p+1)) * v!(v!(p+2)))]) (p+4)
    99 -> v!0

main = do
  prg <- V.fromList . map read . linesBy (== ',') <$> getContents
  print $ evaluate prg 12 2

  -- I initially searched (and found and starred part 2) with a bash
  -- oneliner, arguments between 0 and 100 inclusive.  Then I cleaned
  -- up the code and used a quarter-plane enumeration that worked for
  -- infinite inputs.  Then /u/sophiebits showed me (any many others)
  -- that the input was bounded, so I'm simplifying to that even
  -- though the 2D enumeration was kind of elegant.  (and actually
  -- shorter, lol)
  -- https://old.reddit.com/r/adventofcode/comments/e4u0rw/2019_day_2_solutions/f9f9uow/
  let Just (noun,verb) = find ((== 19690720) . uncurry (evaluate prg))
                         [ (i,j) | i <- [0..99], j <- [0..99] ]
  print $ 100 * noun + verb
