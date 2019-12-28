-- Day 20: Donut Maze
{-# LANGUAGE TupleSections #-}

import           Data.Array
import           Data.Char  (isAlpha)
import           Data.Maybe (mapMaybe,listToMaybe)
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

-- In all seriousness, the parsing is the hardest component of this
-- puzzle.  It wasn't obvious to me at all from the start (and AFAICR,
-- isn't actually disambiguated in the text, only by the examples)
-- that the portals are to be read top-down and left-to-right always,
-- and not in the direction the walking would take you.  Which totally
-- made sense independently on its own, even moreso with AA and ZZ
-- being a duplicated letter.
--
-- My algorithm here just checks for a letter between a dot and
-- another letter.  And reorders the letters @#$!  I think I also had
-- a very stupid off-by-one in my warp type decision.  Stupid, as in
-- correct for i and wrong for j.  D'uh.
findWarps :: Grid -> [((Int, Int), Warp)]
findWarps g = mapMaybe ana (assocs g) where
  ana ((i,j),c) | isAlpha c = listToMaybe
                  [ (pm,(if di+dj > 0 then [c,c'] else [c',c],warpType pm))
                  | (di,dj) <- [(-1,0),(0,1),(1,0),(0,-1)]
                  , let pl = (i+di,j+dj), inRange (bounds g) pl
                  , let c' = g!pl, isAlpha c'
                  , let pm = (i-di,j-dj), inRange (bounds g) pm, g!pm == '.' ]
          | otherwise = Nothing
  ((1,1),(height,width)) = bounds g
  warpType (i,j) | i == 3 || j == 3 || i == height-2 || j == width-2 = Outer
                 | otherwise                                         = Inner

-- Breadth-first search.  At least the third of the month.  The only
-- tricky bit is not to forget to prevent using the outer portals on
-- the outer level.  I implemented this by forbidding negative levels.
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
  go _ [] = error "BFS exhausted. Something's wrong!"
