-- Day 8: Space Image Format

import Data.Ord        (comparing)
import Data.List       (minimumBy,transpose)
import Data.List.Split (chunksOf)

numOf :: Eq a => a -> [a] -> Int
numOf = (length .) . filter . (==)

combine :: Char -> Char -> Char
combine '0' _ = ' '
combine '1' _ = '*'
combine  _  b =  b

bg :: Char
bg = error "Leftover transparent pixels"

main :: IO ()
main = do
  layers <- chunksOf (6 * 25) <$> getLine
  print $ (*) <$> numOf '1' <*> numOf '2'
        $ minimumBy (comparing $ numOf '0') layers
  mapM_ putStrLn $ chunksOf 25 $ map (foldr combine bg) $ transpose layers
