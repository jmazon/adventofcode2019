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

-- Some basic tracing support

traceState, traceOps :: Bool
traceState = False
traceOps = False

traceOp :: Applicative m => [String] -> m ()
traceOp | traceOps = traceM . intercalate ","
        | otherwise = const (pure ())

-- Day 2: the initial IntCode machine

newtype Value = Val { unVal :: Int } deriving (Eq,Ord,Num)
type RAM = Vector Value

getIntCode :: IO RAM
getIntCode = V.fromList . map (Val . read) . linesBy (== ',') <$> getContents

evaluateOld :: RAM -> Int -> Int -> Int
evaluateOld prg i j = unVal . (! 0) $ ram $ fst $ execRWS
  (unM _evaluate)
  (R ())
  (S (prg // [(1,Val i),(2,Val j)]) (error "No input to evaluateOld") 0 0)

newtype M a = M { unM :: RWS R [Int] S a }
            deriving (Functor,Applicative,Monad,
                      MonadState S,MonadWriter [Int])
instance MonadFail M where fail = error

newtype R = R () -- currently unused, but I'll keep the typing for free.

data S = S { ram :: !RAM, inputStream :: [Int]
           , instructionPointer :: !Int, relativeBase :: !Int }

_evaluate :: M ()
_evaluate = whileM $ do
  -- traceState && traceShow (v,r,p) False = undefined
  Op {opCode,paramModes} <- decodeOp <$> readInstr

  case opCode of
    1  -> binOp paramModes (+) "ADD"
    2  -> binOp paramModes (*) "MUL"
    99 -> haltOp paramModes "HCF"

    -- day 5 part 1
    3  -> inOp paramModes "IN"
    4  -> outOp paramModes "OUT"

    -- day 5 part 2
    5  -> condBranchOp paramModes (/= 0) "BNZ"
    6  -> condBranchOp paramModes (== 0) "BZ"
    7  -> binOp paramModes (((Val . fromEnum) .) .  (<)) "LT"
    8  -> binOp paramModes (((Val . fromEnum) .) . (==)) "EQ"

    -- day 9
    9  -> relOp paramModes "INCR"

    x  -> error $ "Unknown opcode " ++ show x
  pure (opCode /= 99)

binOp :: (MonadFail m,MonadState S m) =>
         [Mode] -> (Value -> Value -> Value) -> String -> m ()
binOp modes f name = do
  ([Left op1,Left op2,Right out],dbgOps) <- readParams modes [In,In,Out]
  traceOp (name:dbgOps)
  out (op1 `f` op2)

haltOp :: Applicative m => [Mode] -> String -> m ()
haltOp _ name = traceOp [name]

-- Day 5: I/O and a new ABI

evaluate :: RAM -> [Int] -> [Int]
evaluate prg i = snd $ evalRWS (unM _evaluate) (R ()) (S prg i 0 0)

inOp :: (MonadFail m,MonadState S m) => [Mode] -> String -> m ()
inOp modes name = do
  ([Right out],dbgOps) <- readParams modes [Out]
  traceOp (name:dbgOps)
  s@S{inputStream = (h:t)} <- get
  put $! s { inputStream = t }
  out (Val h)

outOp :: (MonadFail m,MonadState S m,MonadWriter [Int] m)
      => [Mode] -> String -> m ()
outOp modes name = do
  ([Left op1],dbgOps) <- readParams modes [In]
  traceOp (name:dbgOps)
  tell [unVal op1]

-- Also day 5: parameter modes

data Op = Op { opCode :: !Int, paramModes :: [Mode] }

decodeOp :: Value -> Op
decodeOp (Val n) = Op (n `mod` 100) (map (mode (n `div` 100)) [0..2])

data Mode = Position | Immediate | Relative deriving (Show,Enum)

mode :: Int -> Int -> Mode
mode ms n = toEnum $ ms `div` 10^n `mod` 10

data Dir = In | Out

readParams :: MonadState S m =>
              [Mode] -> [Dir] -> m ([Either Value (Value -> m ())],[String])
readParams modes dirs = unzip <$> zipWithM f modes dirs where
  f m In  = fmap (first Left)  .  inParam m =<< readInstr
  f m Out = fmap (first Right) . outParam m =<< readInstr

inParam :: MonadState S m => Mode -> Value -> m (Value,String)
inParam m (Val s) = case m of
    Position  -> readAddr s >>= \v -> pure (v,showPos s)
    Immediate ->                      pure (Val s,show    s)
    Relative  -> readRel  s >>= \v -> pure (v,showRel s)

outParam :: MonadState S m => Mode -> Value -> m (Value -> m (),String)
outParam m (Val t) = case m of
    Position -> (,showPos t) <$> pure (writeAddr t)
    Relative -> (,showRel t) <$>       writeRel  t
    x -> error $ "Invalid output addressing mode " ++ show x

showPos :: Int -> String
showPos p = "[" ++ show p ++ "]"

-- Day 5 part 2: abstract position handling for branching operations

adjIP :: MonadState S m => (Int -> Int) -> m ()
adjIP f = modify' $ \s -> s { instructionPointer = f (instructionPointer s) }

readInstr :: MonadState S m => m Value
readInstr = readAddr =<< gets instructionPointer <* adjIP succ

condBranchOp :: (MonadFail m,MonadState S m) =>
                [Mode] -> (Value -> Bool) -> String -> m ()
condBranchOp modes pr name = do
  ([Left op1,Left (Val op2)],dbgOps) <- readParams modes [In,In]
  traceOp (name:dbgOps)
  when (pr op1) (adjIP (const op2))

-- Day 9: support for relative mode

readRel :: MonadState S m => Int -> m Value
readRel offset = readAddr . (+ offset) =<< gets relativeBase

writeRel :: MonadState S m => Int -> m (Value -> m ())
writeRel offset = writeAddr . (+ offset) <$> gets relativeBase

relOp :: (MonadFail m,MonadState S m) => [Mode] -> String -> m ()
relOp modes name = do
  ([Left (Val delta)],dbgOps) <- readParams modes [In]
  traceOp (name:dbgOps)
  modify' $ \s -> s { relativeBase = relativeBase s + delta }

showRel :: Int -> String
showRel = ('R' :) . showPos

-- Also day 9: memory is now infinite in natural addresses.

readAddr :: MonadState S m => Int -> m Value
readAddr a | a < 0 = error $ "Read from negative address " ++ show a
readAddr addr = fromMaybe 0 . (!? addr) <$> gets ram

writeAddr :: MonadState S m => Int -> Value -> m ()
writeAddr t n | n < 0 = error $ "Write at negative address " ++ show (t,unVal n)
writeAddr t n = do
  s@S{ram = v} <- get
  let v' | t < V.length v = v
         | otherwise = v <> V.replicate (t - V.length v + 1) 0
      v'' = v' // [(t,n)]
  n `seq` v'' `seq` put s { ram = v'' }
