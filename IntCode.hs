-- TODO: eliminate MonadFail (make readParams take static vector)

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module IntCode (getIntCode,evaluateOld,evaluate) where

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Vector (Vector,(!),(!?),(//))
import Data.List.Split (linesBy)
import Data.List (intercalate)
import Control.Arrow
import Control.Monad.RWS.Lazy
import Control.Monad.Fail
import Control.Monad.Extra (whileM)
import Debug.Trace

traceState, traceOps :: Bool
traceState = False
traceOps = False

traceOp :: Applicative m => [String] -> m ()
traceOp | traceOps = traceM . intercalate ","
        | otherwise = const (pure ())

type RAM = Vector Int

getIntCode :: IO RAM
getIntCode = V.fromList . map read . linesBy (== ',') <$> getContents

evaluateOld :: RAM -> Int -> Int -> Int
evaluateOld prg i j = (! 0) $ ram $ fst $ execRWS
                        (unM _evaluate)
                        (R ())
                        (S (prg // [(1,i),(2,j)]) (error "No input to evaluateOld") 0 0)

evaluate :: RAM -> [Int] -> [Int]
evaluate prg i = snd $ evalRWS (unM _evaluate) (R ()) (S prg i 0 0)

newtype M a = M { unM :: RWS R [Int] S a }
            deriving (Functor,Applicative,Monad,
                      MonadState S,MonadWriter [Int])
instance MonadFail M where fail = error

data S = S { ram :: !RAM, inputStream :: [Int]
           , position :: !Int, relative :: !Int }

_evaluate :: M ()
_evaluate = whileM go where
  -- go _ r p | traceState && traceShow (v,r,p) False = undefined
  go = do
    op <- decodeOp <$> readInstr
    let i = modInst op
    case i of
      1  -> binOp op (+) "ADD"
      2  -> binOp op (*) "MUL"
      3  -> inOp op "IN"
      4  -> outOp op "OUT"
      5  -> condBranchOp op (/= 0) "BNZ"
      6  -> condBranchOp op (== 0) "BZ"
      7  -> binOp op ((fromEnum .) .  (<)) "LT"
      8  -> binOp op ((fromEnum .) . (==)) "EQ"
      9  -> relOp op "INCR"
      99 -> haltOp op "HCF"
      x  -> error $ "Unknown opcode " ++ show x
    pure (i /= 99)

binOp :: (MonadFail m,MonadState S m) =>
         Op -> (Int -> Int -> Int) -> String -> m ()
binOp op f name = do
  ([Left op1,Left op2,Right out],dbgOps) <- readParams op [In,In,Out]
  traceOp (name:dbgOps)
  out (op1 `f` op2)

inOp :: (MonadFail m,MonadState S m) => Op -> String -> m ()
inOp op name = do
  ([Right out],dbgOps) <- readParams op [Out]
  traceOp (name:dbgOps)
  s@S{inputStream = (h:t)} <- get
  put $! s { inputStream = t }
  out h

outOp :: (MonadFail m,MonadState S m,MonadWriter [Int] m)
      => Op -> String -> m ()
outOp op name = do
  ([Left op1],dbgOps) <- readParams op [In]
  traceOp (name:dbgOps)
  tell [op1]

condBranchOp :: (MonadFail m,MonadState S m) =>
                Op -> (Int -> Bool) -> String -> m ()
condBranchOp op pr name = do
  ([Left op1,Left op2],dbgOps) <- readParams op [In,In]
  traceOp (name:dbgOps)
  when (pr op1) (setPos op2)

relOp :: (MonadFail m,MonadState S m) => Op -> String -> m ()
relOp op name = do
  ([Left delta],dbgOps) <- readParams op [In]
  traceOp (name:dbgOps)
  advanceRel delta

readInstr :: MonadState S m => m Int
readInstr = readAddr =<< gets position <* incrPos

data Op = Op { modInst :: !Int, opInst :: !Int }

decodeOp :: Int -> Op
decodeOp n = Op (n `mod` 100) (n `div` 100)

haltOp :: Applicative m => Op -> String -> m ()
haltOp _ name = traceOp [name]

readAddr :: MonadState S m => Int -> m Int
readAddr a | a < 0 = error $ "Read from negative address " ++ show a
readAddr addr = fromMaybe 0 . (!? addr) <$> gets ram

writeAddr :: MonadState S m => Int -> Int -> m ()
writeAddr a n | n < 0 = error $ "Write at negative address " ++ show (a,n)
writeAddr t n = do
  s@S{ram = v} <- get
  let v' | t < V.length v = v
         | otherwise = v <> V.replicate (t - V.length v + 1) 0
      v'' = v' // [(t,n)]
  n `seq` v'' `seq` put s { ram = v'' }

data Mode = Position | Immediate | Relative deriving (Show,Enum)

mode :: Op -> Int -> Mode
mode op operand = toEnum $ opInst op `div` 10^(operand-1) `mod` 10

newtype R = R ()

setPos :: MonadState S m => Int -> m ()
setPos p = modify' $ \s -> s { position = p }

incrPos :: MonadState S m => m ()
incrPos = modify' $ \s -> s { position = position s + 1 }

advanceRel :: MonadState S m => Int -> m ()
advanceRel delta = modify' $ \s -> s { relative = relative s + delta }

readRel :: MonadState S m => Int -> m Int
readRel offset = readAddr . (+ offset) =<< gets relative

writeRel :: MonadState S m => Int -> m (Int -> m ())
writeRel offset = writeRel . (+ offset) =<< gets relative

data Dir = In | Out

showPos,showRel :: Int -> String
showPos p = "[" ++ show p ++ "]"
showRel = ('R' :) . showPos

readParams :: MonadState S m =>
              Op -> [Dir] -> m ([Either Int (Int -> m ())],[String])
readParams op dirs = unzip <$> zipWithM f [1..] dirs where
  f n In  = fmap (first Left)  .  inParam (mode op n) =<< readInstr
  f n Out = fmap (first Right) . outParam (mode op n) =<< readInstr

inParam :: MonadState S m => Mode -> Int -> m (Int,String)
inParam m s = case m of
    Position  -> readAddr s >>= \v -> pure (v,showPos s)
    Immediate ->                      pure (s,show    s)
    Relative  -> readRel  s >>= \v -> pure (v,showRel s)

outParam :: MonadState S m => Mode -> Int -> m (Int -> m (),String)
outParam m t = case m of
    Position -> (,showPos t) <$> pure (writeAddr t)
    Relative -> (,showRel t) <$>       writeRel  t
    x -> error $ "Invalid output addressing mode " ++ show x
