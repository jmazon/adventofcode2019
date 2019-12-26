-- Day 7: Amplification Circuit

import IntCode

import Data.List (permutations)
import Data.Function (fix)

-- An amplifier is a transducer, i.e. a stream to stream function.
amplifier :: RAM -> Int -> Transducer
amplifier prg phase is = evaluate prg (phase : is)

-- A series of amplifiers is simply their functional composition.  Yay FP!
series :: RAM -> [Int] -> Transducer
series prg = foldr1 (.) . map (amplifier prg)

-- A feedback loop is built as a series' least fixed point.  Yay lazyness!
loop :: RAM -> [Int] -> Transducer
loop prg phases = fix . (series prg phases .) . (++)

main :: IO ()
main = do
  prg <- getIntCode
  print $ maximum $ map (last . ($ [0]) . series prg) $ permutations [0..4]
  print $ maximum $ map (last . ($ [0]) . loop   prg) $ permutations [5..9]
