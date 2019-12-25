import IntCode
import Data.Char
import Control.Monad

objs = [ "food ration", "fixed point", "weather machine", "semiconductor"
       , "planetoid", "coin", "pointer", "klein bottle" ]

withObjs :: String -> [String] -> String
withObjs action objs = unlines (takes ++ [action] ++ drops) where
  takes = map ("take " ++) objs
  drops = map ("drop " ++) objs

main = do
  prg <- getIntCodeFromFile "day25.in"
  script <- readFile "day25.script"
  remainder <- getContents
  let dropAll = unlines $ map ("drop " ++) objs
      objAttempts = filterM (const [True,False]) objs
      input = script ++ dropAll ++ concatMap (withObjs "north") objAttempts ++ input
      output = map chr $ evaluate prg $ map ord input
  putStr output
