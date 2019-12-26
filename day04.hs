-- Day 4: Secure Container
{-# LANGUAGE LambdaCase #-}

enumerate :: String -> String -> [String]
enumerate low high = (++ [high]) $ takeWhile (/= high) $ iterate next low
  where next ('9':ds) = '0' : next ds
        next ( d :ds) = succ d : ds
        next     _    = error $ "Exhausted numbers enumerating upwards from " ++
                                low ++ " to " ++ high

valid :: Int -> Int -> String -> Bool
valid short long = go False 0 '9' where
  -- {has seen doubled digit} {current run length} {previous digit}
  go db rl p = \case []   -> db' -- reached end nondecreasingly, check double
                     d:ds -> case compare d p of
                       GT -> False -- decrease, abort
                                   -- (remember, we're reading in reverse)
                       EQ -> go db  (rl + 1) d ds -- same, increase run length
                       LT -> go db'     1    d ds -- change, commit run length
                                                  --         if valid
    where db' = db || rl >= short && rl <= long

main :: IO ()
main = do
  (low@[_,_,_,_,_,_],_:high@[_,_,_,_,_,_]) <- break (== '-') <$> getLine
  let candidates = enumerate (reverse low) (reverse high)
  print $ length $ filter (valid 2 6) candidates
  print $ length $ filter (valid 2 2) candidates
