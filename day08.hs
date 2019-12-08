import Data.Function   (on)
import Data.List       (minimumBy,transpose)
import Data.List.Split (chunksOf)

numOf = (length .) .  filter . (==)

combine '0' _ = ' '
combine '1' _ = '*'
combine  _  b =  b

main = do
  layers <- chunksOf (6 * 25) <$> getLine
  print $ (*) <$> numOf '1' <*> numOf '2'
        $ minimumBy (compare `on` numOf '0') layers
  mapM_ putStrLn $ chunksOf 25 $ map (foldr combine '\a') (transpose layers)
