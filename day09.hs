-- Day 9: Sensor Boost

import IntCode

main :: IO ()
main = do
  prg <- getIntCode
  let [boostSignal]       = evaluate prg [1]
      [signalCoordinates] = evaluate prg [2]
  print boostSignal
  print signalCoordinates
