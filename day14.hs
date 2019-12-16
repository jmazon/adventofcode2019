{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map,(!))
import           Control.Monad.RWS.Strict
import           Text.Regex.PCRE

readReaction l = (product,(stoichiometry,ingredients)) where
  ((product,stoichiometry):ingredients) = parseIngredients $
                                          reverse shoppingList
  parseIngredients ((i,n):is) = (i,read n) : parseIngredients is
  parseIngredients [] = []
  shoppingList = zip (getAllTextMatches $ l =~ "[A-Z]+")
                     (getAllTextMatches $ l =~ "[0-9]+")

consume quantity "ORE" = tell (Sum quantity)
consume quantity product = do
  create <- (quantity -) <$> gets (M.findWithDefault 0 product)
  when (create > 0) $ do
    (stoichiometry,ingredients) <- asks (! product)
    let q = (create + stoichiometry - 1) `div` stoichiometry
    forM_ ingredients $ \(i,n) -> consume (q*n) i
    modify' (M.insertWith (+) product (q*stoichiometry))
  modify' (M.insertWith (+) product (-quantity))

bsearch f goal = go where
  go ok excess | excess <= ok+1 = ok
               | f mid <= goal  = go mid excess
               | otherwise      = go ok  mid
    where mid = (ok + excess) `div` 2

main = do
  recipe <- M.fromList . map readReaction . lines <$> getContents
  let oreCost fuel = getSum $ snd $ evalRWS (consume fuel "FUEL") recipe M.empty
      unitOreCost = oreCost 1
      oreCapacity = 1000000000000
      minFuel = last $ takeWhile ((< oreCapacity) . oreCost) $
                iterate (*2) (oreCapacity `div` unitOreCost)
  print unitOreCost
  print $ bsearch oreCost oreCapacity minFuel (2*minFuel)
