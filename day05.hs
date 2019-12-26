-- Day 5: Sunny with a Chance of Asteroids

import IntCode

main :: IO ()
main = do
  prg <- getIntCode

  let [diagnosticCode1] = dropWhile (== 0) $ evaluate prg [1]
  print diagnosticCode1

  let [diagnosticCode2] = evaluate prg [5]
  print diagnosticCode2
