{-# OPTIONS_GHC -Wno-deprecations #-}

-- TODO: eliminate MonadFail (make readParams take static vector)

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module IntCode (RAM,getIntCode,getIntCodeFromFile,evaluateOld,evaluate,evaluateF,evaluateT,IntCodeF(..),Value(..),Transducer) where

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Vector (Vector,(!),(!?),(//))
import Data.List.Split (linesBy)
import Control.Monad.RWS.Lazy
import Control.Monad.State.Strict
import Control.Monad.Fail
import Control.Monad.Error
import Control.Monad.Free.TH
import Control.Monad.Trans.Free
import Control.Monad.Extra (whileM)
import Data.Coerce

type Transducer = [Int] -> [Int]

newtype Address = Addr { unAddr :: Int } deriving (Eq,Ord,Num)
newtype Value = Val { unVal :: Int } deriving (Eq,Ord,Num)
type RAM = Vector Value

-- These are for the free monad interface, but have to be up here for
-- Template Haskell reasons.
data IntCodeF a = Input (Value -> a) | Output Value a deriving Functor
makeFree ''IntCodeF
-- Please pardon the interruption.


-- Day 2: the initial IntCode machine

readIntCode :: String -> RAM
readIntCode = V.fromList . map (Val . read) . linesBy (== ',')

getIntCodeFromFile :: FilePath -> IO RAM
getIntCodeFromFile f = readIntCode <$> readFile f

getIntCode :: IO RAM
getIntCode = readIntCode <$> getContents

evaluateOld :: RAM -> Int -> Int -> Int -- used by day 2
evaluateOld prg i j = unVal . (! 0) $ ram $ fst $ execRWS
  (unM $ runIntCodeInRW evaluateGeneric)
  (error "No input to evaluateOld")
  (S (prg // [(1,Val i),(2,Val j)]) 0 0)

newtype M a = M { unM :: RWS R W S a }
            deriving (Functor,Applicative,Monad,
                      MonadReader R,MonadState S,MonadWriter W)
instance MonadFail M where fail = error

type R = [Value]
type W = [Value]
data S = S { ram :: !RAM
           , instructionPointer :: !Address, relativeBase :: !Address }

evaluateGeneric :: (MonadFree IntCodeF m,MonadFail m,MonadState S m) => m ()
evaluateGeneric = whileM $ do
  Op {opCode,paramModes} <- decodeOp <$> readInstr

  case opCode of
    1  -> binOp paramModes (+)
    2  -> binOp paramModes (*)
    99 -> haltOp paramModes

    -- day 5 part 1
    3  -> inOp paramModes
    4  -> outOp paramModes

    -- day 5 part 2
    5  -> condBranchOp paramModes (/= 0)
    6  -> condBranchOp paramModes (== 0)
    7  -> binOp paramModes (((Val . fromEnum) .) .  (<))
    8  -> binOp paramModes (((Val . fromEnum) .) . (==))

    -- day 9
    9  -> relOp paramModes

    x  -> error $ "Unknown opcode " ++ show (unOpCode x)
  pure (opCode /= 99)

binOp :: (MonadFail m,MonadState S m) =>
         [Mode] -> (Value -> Value -> Value) -> m ()
binOp modes f = do
  [Left op1,Left op2,Right out] <- readParams modes [In,In,Out]
  out (op1 `f` op2)

haltOp :: Applicative m => [Mode] -> m ()
haltOp _ = pure ()

-- Day 5: I/O and a new ABI

evaluate :: RAM -> Transducer
evaluate prg i = coerce $ snd $
  evalRWS (unM $ runIntCodeInRW evaluateGeneric) (coerce i) (S prg 0 0)

inOp :: (MonadFree IntCodeF m,MonadFail m,MonadState S m)
     => [Mode] -> m ()
inOp modes = do
  [Right out] <- readParams modes [Out]
  input >>= out

outOp :: (MonadFree IntCodeF m,MonadFail m,MonadState S m)
      => [Mode] -> m ()
outOp modes = do
  [Left op1] <- readParams modes [In]
  output op1

-- Also day 5: parameter modes

data Op = Op { opCode :: !OpCode, paramModes :: [Mode] }
newtype OpCode = OpCode { unOpCode :: Int } deriving (Eq,Num)

decodeOp :: Value -> Op
decodeOp (Val n) = Op (OpCode (n `mod` 100)) (map mode [0..2])
  where mode i = toEnum $ n `div` 10^(i+2 :: Int) `mod` 10

data Mode = Position | Immediate | Relative deriving (Show,Enum)

data Dir = In | Out

readParams :: MonadState S m =>
              [Mode] -> [Dir] -> m [Either Value (Value -> m ())]
readParams modes dirs = zipWithM f modes dirs where
  f m In  = fmap  Left  .  inParam m =<< readInstr
  f m Out = fmap  Right . outParam m =<< readInstr

inParam :: MonadState S m => Mode -> Value -> m Value
inParam m imm@(Val int) = case m of
    Position  -> readAddr pos
    Immediate -> pure imm
    Relative  -> readRel  pos
  where pos = Addr int

outParam :: MonadState S m => Mode -> Value -> m (Value -> m ())
outParam m (Val int) = case m of
    Position -> pure (writeAddr pos)
    Relative ->       writeRel  pos
    x -> error $ "Invalid output addressing mode " ++ show x
  where pos = Addr int

-- Day 5 part 2: abstract position handling for branching operations

adjIP :: MonadState S m => (Address -> Address) -> m ()
adjIP f = modify' $ \s -> s { instructionPointer = f (instructionPointer s) }

readInstr :: MonadState S m => m Value
readInstr = readAddr =<< gets instructionPointer <* adjIP (+1)

condBranchOp :: (MonadFail m,MonadState S m) =>
                [Mode] -> (Value -> Bool) -> m ()
condBranchOp modes pr = do
  [Left op1,Left (Val op2)] <- readParams modes [In,In]
  when (pr op1) (adjIP (const (Addr op2)))

-- Day 9: support for relative mode

readRel :: MonadState S m => Address -> m Value
readRel offset = readAddr . (+ offset) =<< gets relativeBase

writeRel :: MonadState S m => Address -> m (Value -> m ())
writeRel offset = writeAddr . (+ offset) <$> gets relativeBase

relOp :: (MonadFail m,MonadState S m) => [Mode] -> m ()
relOp modes = do
  [Left (Val delta)] <- readParams modes [In]
  modify' $ \s -> s { relativeBase = relativeBase s + Addr delta }

-- Also day 9: memory is now infinite in natural addresses.

readAddr :: MonadState S m => Address -> m Value
readAddr a | a < 0 = error $ "Read from negative address " ++ show (unAddr a)
readAddr (Addr a) = fromMaybe 0 . (!? a) <$> gets ram

writeAddr :: MonadState S m => Address -> Value -> m ()
writeAddr t n | t < 0 = error $ "Write at negative address " ++
                                show (unAddr t,unVal n)
writeAddr (Addr t) n = do
  s@S{ram = v} <- get
  let v' | t < V.length v = v
         | otherwise = v <> V.replicate (t - V.length v + 1) 0
      v'' = v' // [(t,n)]
  n `seq` v'' `seq` put s { ram = v'' }

-- This would have been really needed to make my breakout (day 13)
-- solution less of a kludge, but I didn't get to implementing in
-- until late day 16.  Mmm.  to *start* implementing it.

-- Anyway: convert interpreter from the stream to stream interface to
-- a free monad one.  But keep the stream interface, of course, it's
-- perfect for e.g. day 5.  Just implement in terms of the free monad
-- one instead.

runIntCodeInRW :: (MonadReader [Value] m,MonadWriter [Value] m)
               => FreeT IntCodeF m a -> m a
runIntCodeInRW = iterT run where
  run (Input f) = local tail . f =<< asks head
  run (Output i f) = tell [i] >> f

evaluateF :: RAM -> Free IntCodeF ()
evaluateF prg = evaluateT prg

evaluateT :: Monad m => RAM -> FreeT IntCodeF m ()
evaluateT prg = fmap (either error id) $ runErrorT $ evalStateT (evaluateGeneric) (S prg 0 0)
