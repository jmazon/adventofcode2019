-- Day 5: Sunny with a Chance of Asteroids

import IntCode (getIntCode,runIntStream)

main :: IO ()
main = do
  prg <- getIntCode

  let [diagnosticCode1] = dropWhile (== 0) $ runIntStream prg [1]
  print diagnosticCode1

  let [diagnosticCode2] = runIntStream prg [5]
  print diagnosticCode2
