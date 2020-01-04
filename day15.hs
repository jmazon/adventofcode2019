-- Day 15: Oxygen System
{-# LANGUAGE NamedFieldPuns #-}

import IntCode (RAM,getIntCode,runAgent)

import           Data.Map.Strict (Map,(!),empty,insert,insertWith)
import qualified Data.Set as S
import           Data.List (foldl')

data Status = Wall | Open | Oxygen | Unknown deriving (Eq,Enum)
data Command = Invalid | North | South | West | East deriving Enum
type Pos = (Int,Int)
type Chart = Map Pos Status

neighbors :: Pos -> [Pos]
neighbors (i,j) = [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]

commandFromDelta :: Pos -> Pos -> Command
commandFromDelta (i0,j0) (i,j) = case (i-i0,j-j0) of
  (-1,0) -> North
  ( 1,0) -> South
  (0,-1) -> West
  (0, 1) -> East
  wtf -> error $ "LULWAT " ++ show wtf

data AgentState = AS { pos :: !Pos, chart :: !Chart }

runRepairDroid :: RAM -> Pos -> Chart
runRepairDroid prg startPos = runAgent prg agent chart state0 where

  -- Exploration strategy: move toward nearest unknown position
  agent :: AgentState -> Maybe (Command,Status -> AgentState)
  agent AS {pos,chart} =
    case bfs chart pos Unknown of -- Search for unknowns
      [] -> Nothing     -- None?  We're done here.
      (path@(_:_):_) -> -- Found nearest one.  Make the first step.
        let step = last path in
        Just ( commandFromDelta pos step
             , \status -> let pos' = case status of
                                Wall    -> pos
                                Unknown -> error "WTF"
                                _       -> step
                          in AS pos' (updateChart chart step status) )
      ([]:_) -> error "Internal error: not supposed to stand on Unknown"

  -- When updating the chart, the position reported by the droid is
  -- overwritten (it's supposed to have been an Unknown); if it was
  -- some kind of open, all its neighbors are new potential places to
  -- explore, so mark them as Unknown.  But don't overwrite!
  updateChart :: Chart -> Pos -> Status -> Chart
  updateChart chart pos status | status == Wall = chart'
                               | otherwise      = chart''
    where chart' = insert pos status chart
          chart'' = foldl' (\c n -> insertWith (flip const) n Unknown c)
                           chart' (neighbors pos)

  -- Seed initial chart with the starting position being Open (it's
  -- not explicit in the statement, but it is undoubtedly assumed by
  -- the example.  It also happens to match my input.
  state0 = AS startPos (updateChart empty startPos Open)

-- This BFS returns all paths to all goals, by increasing order of
-- distance.  So it can be used for all path-related parts of this
-- problem:
--   • exploration: take first step toward first goal
--   • shortest path length: length of first goal's path
--   • flood fill time: length of last goal's path
-- Lazy evaluation lets this be reasonably efficient.
bfs :: Chart -> Pos -> Status -> [[Pos]]
bfs chart start goal = go S.empty [[start]] where
  go _ [] = []
  go cl (path@ ~(p:_):q) | p `S.member` cl =           go cl   q
                         | otherwise       = accGoal $ go cl' (q ++ q')
    where cl' = S.insert p cl
          q' = map (: path) $ filter ((/= Wall) . (chart !)) $ neighbors p
          accGoal = if chart ! p == goal then (init path :) else id

main :: IO ()
main = do
  prg <- getIntCode
  let chart = runRepairDroid prg startPos
      startPos = (0,0) -- arbitrary — I just need one
      [pathToOxygen@(oxygenPos:_)] = bfs chart startPos Oxygen
  print $ length pathToOxygen
  print $ length $ last $ bfs chart oxygenPos Open
