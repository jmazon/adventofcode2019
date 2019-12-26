-- Day 1: The Tyranny of the Rocket Equation

fuelRequirements :: Int -> Int
fuelRequirements mass = mass `div` 3 - 2

main :: IO ()
main = do
  moduleMasses <- map read . lines <$> getContents
  print $ sum $ map fuelRequirements moduleMasses
  print $ sum
        $ concatMap (takeWhile (> 0) . tail . iterate fuelRequirements)
          moduleMasses
