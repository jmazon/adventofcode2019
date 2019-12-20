{-# LANGUAGE TupleSections #-}

import           Data.Array
import           Data.Char (isAlpha)
import           Data.Maybe
import qualified Data.Set as S

main :: IO ()
main = do
  raw <- lines <$> getContents
  let height = length raw
      width = length (head raw)
      grid = listArray ((1,1),(height,width)) (concat raw) :: Grid
      warps = findWarps grid
      [(startPos,_)] = filter ((== ("AA",Outer)) . snd) warps
      [(endPos,  _)] = filter ((== ("ZZ",Outer)) . snd) warps
  print $ bfs grid warps False (startPos,0) (endPos,0)
  print $ bfs grid warps True  (startPos,0) (endPos,0)

type Pos = (Int,Int)
type Grid = Array (Int, Int) Char
type Warp = (String,Dir)
data Dir = Outer | Inner deriving Eq

neighbors :: Pos -> [Pos]
neighbors (i,j) = [(i-1,j),(i,j+1),(i+1,j),(i,j-1)]

opposite :: Dir -> Dir
opposite Outer = Inner
opposite Inner = Outer

nesting :: Dir -> Int
nesting Inner =  1
nesting Outer = -1

findWarps :: Grid -> [((Int, Int), Warp)]
findWarps g = mapMaybe ana (assocs g) where
  ana (p@(i,j),c) | isAlpha c = listToMaybe
                    [ (pm,(if di+dj > 0 then [c,c'] else [c',c],warpType pm))
                    | (di,dj) <- [(-1,0),(0,1),(1,0),(0,-1)]
                    , let pl = (i+di,j+dj), inRange (bounds g) pl
                    , let c' = g!pl, isAlpha c'
                    , let pm = (i-di,j-dj), inRange (bounds g) pm, g!pm == '.' ]
            | otherwise = Nothing
  ((1,1),(height,width)) = bounds g
  warpType (i,j) | i == 3 || j == 3 || i == height-2 || j == width-2 = Outer
                 | otherwise                                         = Inner

bfs :: Grid -> [(Pos,Warp)] -> Bool -> (Pos,Int) -> (Pos,Int) -> Int
bfs g ws nest st0 stg = go S.empty [(st0,0)] where
  go cl ((st@(p,dp),d):q)
    | st `S.member` cl = go cl q
    | st == stg = d
    | dp < 0 || g!p /= '.' = go cl q
    | otherwise = go cl' (q ++ q')
    where cl' = S.insert st cl
          q' = map ((,d+1) . warp) $ neighbors p
          warp p' | isAlpha c, Just (wz,dir) <- lookup p ws
                  , dp > 0 || dir == Inner
                  , [(p'',_)] <- filter ((== (wz,opposite dir)) . snd) ws
                              = (p'',dp + nesting dir * fromEnum nest)
                  | otherwise = (p',dp)
            where c = g!p'
