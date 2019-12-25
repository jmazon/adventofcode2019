import IntCode

main :: IO ()
main = do
  prg <- getIntCode
  print $ evaluate prg [1]
  print $ evaluate prg [5]
