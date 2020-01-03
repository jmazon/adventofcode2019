-- Day 9: Sensor Boost

import IntCode (getIntCode,runIntStream)

main :: IO ()
main = do
  prg <- getIntCode
  let [boostSignal]       = runIntStream prg [1]
      [signalCoordinates] = runIntStream prg [2]
  print boostSignal
  print signalCoordinates
