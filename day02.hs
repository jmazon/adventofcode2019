-- Day 2: 1202 Program Alarm

import IntCode (getIntCode,runPeekPoke)

main :: IO ()
main = do
  prg <- getIntCode
  -- I'm only noticing now, at time of cleanup/rewrite, why those two
  -- numbers happen to be the program's parameters.  Damn you,
  -- braindead date format!
  print $ runPeekPoke prg 12 2

  -- I initially searched (and found and starred part 2) with a bash
  -- oneliner, arguments between 0 and 100 inclusive.  Then I cleaned
  -- up the code and used a quarter-plane enumeration that worked for
  -- infinite inputs.  Then /u/sophiebits showed me (any many others)
  -- that the input was bounded, so I'm simplifying to that even
  -- though the 2D enumeration was kind of elegant.  (and actually
  -- shorter, lol)
  -- https://old.reddit.com/r/adventofcode/comments/e4u0rw/2019_day_2_solutions/f9f9uow/
  let [(noun,verb)] = [ (n,v) | n <- [0..99], v <- [0..99]
                              , runPeekPoke prg n v == 19690720 ]
  print $ 100 * noun + verb
