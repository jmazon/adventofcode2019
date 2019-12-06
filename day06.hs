import Data.Maybe
import Data.Tree
import qualified Data.Map as M

readOrbit l = (sun,[sat]) where (sun,_:sat) = break (== ')') l

main = do
  orbits <- map readOrbit . lines <$> getContents
  let sats = M.fromListWith (++) orbits
  let f = unfoldTree (\s -> (s,M.findWithDefault [] s sats)) "COM"
  print $ sum $ zipWith (*) [0..] $ map length $ levels f
  print $ foldTree (common "YOU" "SAN") f

common a b obj sats | obj == a || obj == b = case sats' of
                        [Left d] -> Just (Right (d-1)) -- unlikely it's needed
                        []       -> Just (Left 0)
                    | otherwise = case sats' of
                        []                -> Nothing
                        [Left da,Left db] -> Just (Right (da+db))
                        [Left d]          -> Just (Left (d+1))
                        [Right d]         -> Just (Right d)
  where sats' = catMaybes sats
