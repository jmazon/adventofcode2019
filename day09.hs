import IntCode

main = do
  prg <- getIntCode
  print $ evaluate prg [1]
  print $ evaluate prg [2]
