{-# LANGUAGE FlexibleContexts #-}
import IntCode

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.RWS.Lazy

data Status = Wall | Open | Oxygen | Unknown deriving (Eq,Enum)

data Command = Invalid | North | South | West | East deriving Enum
type Pos = (Int,Int)

neighbors :: Pos -> [Pos]
neighbors (i,j) = [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]

command :: Pos -> Pos -> Command
command (i0,j0) (i,j) = case (i-i0,j-j0) of (-1,0) -> North
                                            ( 1,0) -> South
                                            (0,-1) -> West
                                            (0, 1) -> East
                                            wtf -> error $ "LULWAT " ++ show wtf

droid :: Pos -> [Int] -> (M.Map Pos Status,[Int])
droid startPos statuses = execRWS (explore undefined startPos Open)
                                  statuses M.empty

-- Exploration strategy: if target isn't a block, move there and mark
-- unknown neighbor cells.  Order a move toward nearest yet unknown
-- (which isn't necessarily a neighbor anymore).
explore :: Pos -> Pos -> Status -> RWS [Int] [Int] (M.Map Pos Status) ()
explore prev target status = do
  modify' (M.insert target status)
  pos <- case status of Wall -> pure prev
                        _ -> do forM_ (neighbors target) $ \p -> modify' $
                                  M.insertWith (flip const) p Unknown
                                pure target
  chart <- get
  case bfs chart pos Unknown of
    (path:_) -> do let step = last path
                   tell [fromEnum (command pos step)]
                   status' <- toEnum <$> asks head
                   local tail $ explore pos step status'
    [] -> pure ()

-- This BFS returns all paths to all goals, by increasing order of
-- distance.  So it can be used for all path-related parts of this
-- problem:
--   * exploration: take first step toward first goal
--   * shortest path length: length of first goal's path
--   * flood fill time: length of last goal's path
-- Lazy evaluation lets this be reasonably efficient.
bfs :: M.Map Pos Status -> Pos -> Status -> [[Pos]]
bfs chart start goal = go S.empty [[start]] where
  go _ [] = []
  go cl (path@ ~(p:_):q)
    | p `S.member` cl =           go cl   q
    | otherwise       = accGoal $ go cl' (q ++ q')
    where cl' = S.insert p cl
          q' = map (: path) $ filter ((/= Wall) . (chart M.!)) $ neighbors p
          accGoal = if chart M.! p == goal then (init path :) else id

main :: IO ()
main = do
  prg <- getIntCode
  let statusStream = evaluate prg commandStream
      (chart,commandStream) = droid startPos statusStream
      startPos = (0,0)
      [pathToOxygen@(oxygenPos:_)] = bfs chart startPos Oxygen
  print $ length pathToOxygen
  print $ length $ last $ bfs chart oxygenPos Open
