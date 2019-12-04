enumerate low high = (++ [high]) $ takeWhile (/= high) $ iterate next low where
  next ('9':ds) = '0' : next ds
  next ( d :ds) = succ d : ds
valid short long = go False 0 '9' where
  go db rl m ds = case ds of [] -> db'
                             d:ds -> case compare d m of
                               GT -> False
                               EQ -> go db  (rl + 1) (min d m) ds
                               LT -> go db'     1    (min d m) ds
    where db' = db || rl >= short && rl <= long
main = do
  (low,_:high) <- break (== '-') <$> getLine
  let candidates = enumerate (reverse low) (reverse high)
  print $ length $ filter (valid 2 6) candidates
  print $ length $ filter (valid 2 2) candidates
