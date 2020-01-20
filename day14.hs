-- Day 14: Space Stoichiometry
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map,(!))
import           Control.Monad.RWS.Strict
import           Text.Regex.PCRE
import           Numeric.Search
import           Prelude hiding (product)

type Chemical = String
type Reactants = [(Chemical,Int)]
type Grimoire = Map Chemical (Int,Reactants)

readReaction :: String -> (Chemical,(Int,Reactants))
readReaction l = (product,(stoichiometry,reactants)) where
  ((product,stoichiometry):reactants) = parseReactants $ reverse shoppingList
  parseReactants ((i,n):is) = (i,read n) : parseReactants is
  parseReactants      []    =           []
  shoppingList = zip (getAllTextMatches $ l =~ "[A-Z]+")
                     (getAllTextMatches $ l =~ "[0-9]+")

-- `produce` recursively produces the specified quantity of the
-- specified reagent.  This is guaranteed to be well-defined as there
-- is only one reaction to produce it.  It's not guaranteed to
-- terminate in the current state of the problem statement, as there
-- could be loops in the reaction tree.  But it doesn't happen here,
-- this is AoC, not CodeJam.  (this problem is very similar to GCJ
-- 2018 round 1B problem C “Transmutation”
-- https://codingcompetitions.withgoogle.com/codejam/round/0000000000007764/000000000003675c)
-- Reader: the reaction grimoire
-- Writer: the amount of ORE consumed
-- State: the current amount of stock of each reagent
produce :: ( MonadReader Grimoire m
           , MonadWriter (Sum Int) m
           , MonadState (Map Chemical Int) m )
        => Int -> Chemical -> m ()
produce quantity "ORE" = tell (Sum quantity)
produce quantity product = do
  create <- (quantity -) <$> gets (M.findWithDefault 0 product)
  when (create > 0) $ do
    (stoichiometry,reactants) <- asks (! product)
    let q = (create + stoichiometry - 1) `div` stoichiometry
    forM_ reactants $ \(i,n) -> produce (q*n) i
    modify' (M.insertWith (+) product (q*stoichiometry))
  modify' (M.insertWith (+) product (-quantity))

oreCost :: Grimoire -> Int -> Chemical -> Int
oreCost grimoire quantity reagent =
  getSum $ snd $ evalRWS (produce quantity reagent) grimoire M.empty

main :: IO ()
main = do grimoire <- M.fromList . map readReaction . lines <$> getContents
          let fuelOreCost q = oreCost grimoire q "FUEL"
              unitOreCost = fuelOreCost 1
          print unitOreCost

          let oreCapacity = 1000000000000
              minFuel = oreCapacity `div` unitOreCost
              Just fuelMargin = largest True $
                                search nonNegativeExponential divForever $
                                \m -> fuelOreCost (minFuel + m) <= oreCapacity
          print (minFuel + fuelMargin)
