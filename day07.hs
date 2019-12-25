import IntCode

import Data.List (permutations)
import Data.Function (fix)

amplifier :: RAM -> Int -> Transducer
amplifier prg p is = evaluate prg (p : is)

chain :: RAM -> [Int] -> Transducer
chain prg = foldr1 (.) . map (amplifier prg)

loop :: RAM -> [Int] -> Transducer
loop prg is ps = fix $ chain prg ps . (is ++)

main :: IO ()
main = do
  prg <- getIntCode
  print $ maximum $ map (flip (chain prg) [0]) $ permutations [0..4]
  print $ maximum $ map (last . loop prg [0]) $ permutations [5..9]
