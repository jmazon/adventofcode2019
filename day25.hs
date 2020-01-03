-- Day 25: Cryostasis

import IntCode (getIntCode,runAscii)
import Data.Maybe
import Control.Monad

-- Scripted: collect the eight free items, ending up south of the pressure plate.
script :: [String]
script = [ "west"
         , "take semiconductor"
         , "west"
         , "take planetoid"
         , "west"
         , "take food ration"
         , "west"
         , "take fixed point"
         , "west"
         , "take klein bottle"
         , "east"
         , "south"
         , "west"
         , "take weather machine"
         , "east"
         , "north"
         , "east"
         , "east"
         , "south"
         , "south"
         , "south"
         , "take pointer"
         , "north"
         , "north"
         , "east"
         , "take coin"
         , "east"
         , "north"
         , "east" ]

main :: IO ()
main = do
  prg <- getIntCode

  -- Computed: try all combinations of object drops until one ends the program.
  let objs = mapMaybe ( fmap (unwords . tail) . mfilter ((== "take") . head) .
                        pure . words ) script
  putStrLn $ last $ lines $ runAscii prg $ unlines $
             script ++ concatMap (withoutObjs "north")
                                 (filterM (const [False,True]) objs)

withoutObjs :: String -> [String] -> [String]
withoutObjs action objs = map ("drop " ++) objs ++ [action]
                       ++ map ("take " ++) objs
