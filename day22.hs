main = do
  shuffle <- map words . lines <$> getContents

  let m = 10007
      (a,b) = foldr (compute m) (1,0) shuffle
  print $ (2019 - b) * inv m a `mod` m

  let m = 119315717514047
      (a,b) = pow (mult m) (foldr (compute m) (1,0) shuffle) 101741582076661
  print $ (2020*a + b) `mod` m

compute m [_,"into",_,_]  (a,b) = (((-1) * a) `mod` m,(-1 - b) `mod` m)
compute m ["cut",     _d] (a,b) = (a `mod` m,(b+read _d) `mod` m)
compute m [_,"with",_,_d] (a,b) = (a*inv m (read _d) `mod` m,b*inv m (read _d) `mod` m)

pow mult z 1 = z
pow mult z n | odd n = mult z (pow mult z (n-1))
             | otherwise = mult p' p'
  where p' = pow mult z (n `div` 2)

mult m (a,b) (c,d) = ((c*a) `mod` m,(b*c+d) `mod` m)

inv m z = pow (((`mod` m) .) . (*)) z (m-2)
