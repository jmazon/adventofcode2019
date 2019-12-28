-- Day 22: Slam Shuffle

import Data.Int (Int32)

main :: IO ()
main = do
  shuffle <- map words . lines <$> getContents

  let m = 10007 :: Int32
      (a,b) = foldr (technique m) (1,0) shuffle
  print $ (2019 - b) * inv m a `mod` m

  let m' = 119315717514047 :: Integer -- Int128 should be enough
      (a',b') = pow (mult m') (foldr (technique m') (1,0) shuffle)
                    101741582076661
  print $ (2020*a' + b') `mod` m'

technique :: (Read i,Integral i) => i -> [String] -> (i,i) -> (i,i)
technique m [_,"into",_, _] (a,b) = ((-a) `mod` m,(-1 - b) `mod` m)
technique m ["cut",     _d] (a,b) = (a `mod` m,(b+read _d) `mod` m)
technique m [_,"with",_,_d] (a,b) = (a*inv m (read _d) `mod` m,b*inv m (read _d) `mod` m)
technique _        ws         _   = error $ "Unknown technique: " ++ unwords ws

pow :: (t -> t -> t) -> t -> Int -> t
pow op = go where go z 1             = z
                  go z n | odd n     = z  `op` go z (n-1)
                         | otherwise = p' `op` p'
                    where p' = pow op z (n `div` 2)

mult :: Integral i => i -> (i,i) -> (i,i) -> (i,i)
mult m (a,b) (c,d) = (c*a `mod` m,(b*c+d) `mod` m)

inv :: Integral i => i -> i -> i
inv m z = pow (((`mod` m) .) . (*)) z (fromIntegral m-2)
