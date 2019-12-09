import IntCode

import Data.List (permutations)
import Data.Function (fix)

amplifier prg p is = evaluate prg (p : is)

chain prg ps is = foldr1 (.) (map (amplifier prg) ps) is

loop prg is ps = fix $ chain prg ps . (is ++)

main = do
  prg <- getIntCode
  print $ maximum $ map (flip (chain prg) [0]) $ permutations [0..4]
  print $ maximum $ map (last . loop prg [0]) $ permutations [5..9]
