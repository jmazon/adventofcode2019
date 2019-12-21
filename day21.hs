import IntCode
import Data.Char

main = do
  prg <- getIntCode
  print $ last $ evaluate prg (encode prg1)
  print $ last $ evaluate prg (encode prg2)

encode = map ord

-- prg1: J = D & !(A&B&C)
prg1 = unlines [ "NOT D J"
               , "NOT J J"
               , "NOT A T"
               , "NOT T T"
               , "AND B T"
               , "AND C T"
               , "NOT T T"
               , "AND T J"
               , "WALK" ]

-- prg2: J = D & (E|H) & !(A&B&C)
prg2 = unlines [ "NOT E J"
               , "NOT J J"
               , "OR H J"
               , "AND D J"
               , "NOT A T"
               , "NOT T T"
               , "AND B T"
               , "AND C T"
               , "NOT T T"
               , "AND T J"
               , "RUN" ]
