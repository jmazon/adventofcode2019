-- Day 21: Springdroid Adventure

import IntCode (getIntCode,runAscii')

main :: IO ()
main = do
  prg <- getIntCode
  let (_,[hullDamage1]) = runAscii' prg prg1
      (_,[hullDamage2]) = runAscii' prg prg2
  print hullDamage1
  print hullDamage2

prg1,prg2 :: String

-- I've heard it was possible to optimize these using the fact the
-- registers remain (or clear, I don't remember) between two runs of
-- its logic.  I got my stars with logic that unconditionally
-- overwrote them, and I don't think there's much point in changing
-- that now.

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
