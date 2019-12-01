fuel n = n `div` 3 - 2
main = do
  ms <- map read . lines <$> getContents
  print $ sum (map fuel ms)
  print $ sum (concatMap (tail . takeWhile (> 0) . iterate fuel) ms)
