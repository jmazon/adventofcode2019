import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import           Data.Map.Strict ((!))
import qualified Data.Set as S

readPath = wordsBy (== ',')

wirePath :: [String] -> [((Int,Int),Int)]
wirePath = concat . snd . mapAccumL adv ((0,0),0) where
  adv ((x,y),a) (d:_l) = ( ((x+l*dx,y+l*dy),a+l)
                         , [ ((x+i*dx,y+i*dy),a+i) | i <- [1..l] ])
    where (dx,dy) = case d of 'R' -> (0,1)
                              'L' -> (0,-1)
                              'U' -> (-1,0)
                              'D' -> (1,0)
          l = read _l

dist (x,y) = abs x + abs y

main = do
  w1 <- M.fromListWith min . wirePath . readPath <$> getLine
  w2 <- M.fromListWith min . wirePath . readPath <$> getLine
  let ints = S.elems $ S.intersection (M.keysSet w1) (M.keysSet w2)
  print $ minimum $ map dist ints
  print $ minimum $ map (\i -> w1!i + w2!i) ints
