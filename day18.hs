{-# OPTIONS_GHC -Wall -Wno-deprecations #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Array
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Control.Arrow
import Control.Monad.State.Strict
import Control.Monad.List
import Data.Tuple

main :: IO ()
main = do
  contents <- lines <$> getContents
  let height = length contents
      width = length (head contents)
      grid = listArray ((1,1),(height,width)) (map readCell $ concat contents)
  print (height,width)
  print $ solve True grid

type Pos = (Int,Int)
type Grid = Array Pos Cell
newtype KeyId = KeyId { encodeKey :: Int } deriving (Eq,Ord)
newtype KeySet = KeySet Int deriving (Eq,Ord)
newtype Cell = Cell (Maybe (Maybe Elem)) deriving Eq
data Elem = Key !KeyId | Door !KeyId | Start deriving Eq

charFromKey :: KeyId -> Char
charFromKey (KeyId i) = toEnum (fromEnum 'a' + i)

keyFromChar :: Char -> KeyId
keyFromChar = KeyId . (subtract $ fromEnum 'a') . fromEnum

ksEmpty :: KeySet -> Bool
ksEmpty (KeySet ks) = ks == 0

ksDelete :: KeyId -> KeySet -> KeySet
ksDelete k (KeySet ks) = KeySet (clearBit ks (encodeKey k))

ksElem :: KeyId -> KeySet -> Bool
ksElem k (KeySet ks) = testBit ks (encodeKey k)

makeKeySet :: [KeyId] -> KeySet
makeKeySet = KeySet . foldl' setBit 0 . map encodeKey

elemFromDoor,elemFromKey :: KeyId -> Elem
elemFromDoor = Door
elemFromKey = Key

doorFromElem,keyFromElem :: Elem -> Maybe KeyId
doorFromElem (Door i) = Just i
doorFromElem _ = Nothing
keyFromElem (Key i) = Just i
keyFromElem _ = Nothing

elemFromCell :: Cell -> Maybe Elem
elemFromCell (Cell c) = join c

doorFromCell,keyFromCell :: Cell -> Maybe KeyId
doorFromCell = doorFromElem <=< elemFromCell
keyFromCell = keyFromElem <=< elemFromCell

readCell :: Char -> Cell
readCell '#' = Cell $ Nothing
readCell '.' = Cell $ Just Nothing
readCell '@' = startCell
readCell  c | isAlpha c = Cell $ Just $ Just $
                          (if isUpper c then elemFromDoor else elemFromKey)
                          (keyFromChar (toLower c))
readCell  c  = error $ "Unknown input: " ++ [c]

cellIsWall :: Cell -> Bool
cellIsWall (Cell c) = isNothing c

wallCell,startCell :: Cell
wallCell = Cell Nothing
startCell = Cell (Just (Just Start))

filterKeys :: [Elem] -> [KeyId]
filterKeys = mapMaybe keyFromElem

startElem :: Elem
startElem = Start

solve :: Bool -> Grid -> (Int,[Char])
solve multiply g0 = second (map charFromKey) $
                   evalState (search startPoss keys) M.empty where
  Just (si,sj) = lookup startCell (map swap (assocs g0))

  g | multiply = g0 // [((si-1,sj-1),startCell),((si-1,sj),wallCell),((si-1,sj+1),startCell)
                       ,((si,sj-1),wallCell),((si,sj),wallCell),((si,sj+1),wallCell)
                       ,((si+1,sj-1),startCell),((si+1,sj),wallCell),((si+1,sj+1),startCell)]
    | otherwise = g0

  elemPoss :: [(Elem,Pos)]
  elemPoss = mapMaybe (uncurry (liftM2 (flip (,))) <<< pure *** elemFromCell) (assocs g)
  keys = makeKeySet $ filterKeys $ map fst elemPoss

  startPoss = V.fromList $ map snd $ filter ((== startElem) . fst) elemPoss

  search,search' :: MonadState (M.Map (V.Vector Pos,KeySet) (Int,[KeyId])) m
         => V.Vector Pos -> KeySet -> m (Int,[KeyId])
  search ps ks | ksEmpty ks = pure (0,[])
          | otherwise = do
    cache <- gets (M.lookup (ps,ks))
    case cache of
      Just v -> return v
      Nothing -> do
        v <- search' ps ks
        modify' (M.insert (ps,ks) v)
        return v
  search' ps ks = fmap (minimum . ((1000000,undefined) :)) $ runListT $ do
    (d,k,i) <- ListT $ pure $ bfs ps ks
    let Just kPos = lookup (elemFromKey k) elemPoss
    fmap ((+d) *** (k:)) $
      lift (search (ps V.// [(i,kPos)]) (ksDelete k ks))

  bfs :: V.Vector Pos -> KeySet -> [(Int,KeyId,Int)]
  bfs ps0 ks = go S.empty (zipWith toNode (V.toList ps0) [0..]) where
    toNode p i = ([p],i)
    go :: S.Set Pos -> [([Pos],Int)] -> [(Int,KeyId,Int)]
    go _ [] = []
    go cl ((path@(p:_),i):q) -- | traceShow ks False = undefined
                | p `S.member` cl = go cl q
                | Just k <- keyFromCell c, k `ksElem` ks =
                    (length path - 1,k,i) : go cl' q'
                | cellIsWall c = go cl' q
                | Just d <- doorFromCell c, d `ksElem` ks = go cl' q
                | otherwise = go cl' q'
      where cl' = S.insert p cl
            c = g!p
            q' = q ++ map ((,i) . (: path)) (filter (`S.notMember` cl) (neighbors p))
          
neighbors :: Pos -> [Pos]
neighbors (i,j) = [(i-1,j),(i,j+1),(i+1,j),(i,j-1)]
