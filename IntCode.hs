{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IntCode (
    RAM
  , getIntCode
  , getIntCodeFromFile
  , poke

  , runPeekPoke

  , Transducer
  , runIntStream

  , IntCodeF(..)
  , runIntF
  , runIntT

  , runAscii
  , runAscii'
  ) where

import Data.Char (chr,ord)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector,(!),(!?),(//))
import Data.Vector.Unboxed.Deriving
import Data.List.Split (linesBy)
import Control.Arrow (first)
import Control.Monad.RWS.Lazy
import Control.Monad.State.Strict
import Control.Monad.Trans.Free
import Control.Monad.Extra (whileM)
import Data.Coerce

newtype Address = Addr Int deriving (Eq,Ord,Num)
newtype Value = Val Int deriving (Eq,Ord,Num)
newtype RAM = RAM (Vector Value)

derivingUnbox "Value" [t| Value -> Int |] [| \(Val v) -> v |] [| Val |]

-- Day 2: the initial IntCode machine

readIntCode :: String -> RAM
readIntCode = RAM . V.fromList . map (Val . read) . linesBy (== ',')

getIntCodeFromFile :: FilePath -> IO RAM
getIntCodeFromFile f = readIntCode <$> readFile f

getIntCode :: IO RAM
getIntCode = readIntCode <$> getContents

poke :: RAM -> [(Int,Int)] -> RAM
poke (RAM r) updates = RAM (r // coerce updates)

runPeekPoke :: RAM -> Int -> Int -> Int -- used by day 2
runPeekPoke prg i j = v where
  Val v = r ! 0
  RAM r = ram $ fst $ execRWS (runIntCodeInRW runGeneric)
                              (error "runPeekPoke isn't allowed input.")
                              (S (poke prg [(1,i),(2,j)]) 0 0)

data S = S { ram :: !RAM
           , instructionPointer :: !Address, relativeBase :: !Address }

runGeneric :: (MonadFree IntCodeF m,MonadState S m) => m ()
runGeneric = whileM $ do
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

binOp :: MonadState S m => [Mode] -> (Value -> Value -> Value) -> m ()
binOp modes op = do
  PReadVal opLeft :< PReadVal opRight :< PStore store :< () <- readParams modes
  store (opLeft `op` opRight)

haltOp :: Applicative m => [Mode] -> m ()
haltOp _ = pure ()

-- Day 5: I/O and a new ABI

type Transducer = [Int] -> [Int]

runIntStream :: RAM -> Transducer
runIntStream prg i = coerce $ snd $
  evalRWS (runIntCodeInRW runGeneric) (coerce i) (S prg 0 0)

inOp :: (MonadFree IntCodeF m,MonadState S m) => [Mode] -> m ()
inOp modes = do PStore store :< () <- readParams modes
                store . Val =<< input

outOp :: (MonadFree IntCodeF m,MonadState S m) => [Mode] -> m ()
outOp modes = do PReadVal (Val operand) :< () <- readParams modes
                 output operand

-- Also day 5: parameter modes

data Op = Op { opCode :: !OpCode, paramModes :: [Mode] }
newtype OpCode = OpCode { unOpCode :: Int } deriving (Eq,Num)

decodeOp :: Value -> Op
decodeOp (Val n) = Op (OpCode (n `mod` 100)) (map mode [0..2])
  where mode i = toEnum $ n `div` 10^(i+2 :: Int) `mod` 10

data Mode = Position | Immediate | Relative deriving (Show,Enum)

class Param p where readParam :: MonadState S m => Mode -> Value -> m p

newtype PReadVal = PReadVal Value
instance Param PReadVal where
  readParam m imm@(Val int) = fmap PReadVal $ case m of
      Position  -> readAddr pos
      Immediate -> pure imm
      Relative  -> readRel  pos
    where pos = Addr int

newtype PStore m = PStore ( Value -> m () )
instance MonadState S m => Param (PStore m) where
  readParam m (Val int) = fmap PStore $ case m of
     Position -> pure (writeAddr pos)
     Relative ->       writeRel  pos
     x -> error $ "Invalid output addressing mode " ++ show x
   where pos = Addr int

-- Day 5 part 2: abstract position handling for branching operations

adjIP :: MonadState S m => (Address -> Address) -> m ()
adjIP f = modify' $ \s -> s { instructionPointer = f (instructionPointer s) }

readInstr :: MonadState S m => m Value
readInstr = readAddr =<< gets instructionPointer <* adjIP (+1)

condBranchOp :: MonadState S m => [Mode] -> (Value -> Bool) -> m ()
condBranchOp modes predicate = do
  PReadVal condition :< PReadVal (Val delta) :< () <- readParams modes
  when (predicate condition) (adjIP (const (Addr delta)))

-- Day 9: support for relative mode

readRel :: MonadState S m => Address -> m Value
readRel offset = readAddr . (+ offset) =<< gets relativeBase

writeRel :: (MonadState S m,MonadState S m') => Address -> m (Value -> m' ())
writeRel offset = writeAddr . (+ offset) <$> gets relativeBase

relOp :: MonadState S m => [Mode] -> m ()
relOp modes = do
  PReadVal (Val delta) :< () <- readParams modes
  modify' $ \s -> s { relativeBase = relativeBase s + Addr delta }

-- Also day 9: memory is now infinite in natural addresses.

readAddr :: MonadState S m => Address -> m Value
readAddr (Addr a) | a < 0 = error $ "Read from negative address " ++ show a
readAddr (Addr a) = fmap (\(RAM r) -> fromMaybe 0  (r !? a)) (gets ram)

writeAddr :: MonadState S m => Address -> Value -> m ()
writeAddr (Addr t) (Val n) | t < 0 = error $ "Write at negative address " ++
                                             show (t,n)
writeAddr (Addr t) n = do
  s@S{ram = RAM v} <- get
  let v' | t < V.length v = v
         | otherwise = v <> V.replicate (t - V.length v + 1) 0
      v'' = v' // [(t,n)]
  n `seq` v'' `seq` put s { ram = RAM v'' }

-- This would have been really needed to make my breakout (day 13)
-- solution less of a kludge, but I didn't get to implementing in
-- until late day 16.  Mmm.  to *start* implementing it.

-- Anyway: convert interpreter from the stream to stream interface to
-- a free monad one.  But keep the stream interface, of course, it's
-- perfect for e.g. day 5.  Just implement in terms of the free monad
-- one instead.

data IntCodeF a = Input (Int -> a) | Output Int a deriving Functor

input :: MonadFree IntCodeF m => m Int
input = wrap (Input pure)

output :: MonadFree IntCodeF m => Int -> m ()
output v = wrap (Output v (return ()))

runIntCodeInRW :: (MonadReader [Value] m,MonadWriter [Value] m)
               => FreeT IntCodeF m a -> m a
runIntCodeInRW = iterT run where
  run (Input f) = asks head >>= \(Val v) -> local tail (f v)
  run (Output i f) = tell [Val i] >> f

runIntF :: RAM -> Free IntCodeF ()
runIntF = runIntT

runIntT :: Monad m => RAM -> FreeT IntCodeF m ()
runIntT prg = evalStateT runGeneric (S prg 0 0)

-- I had a generalized function to resolve operation parameters:
-- readParams :: [Mode] -> [Dir] -> m [Either Value (Value -> m ())]
-- It gracefully handled all number of operands from 0 (well, unused,
-- but it would have worked) to 4 or more.  At the cost of a
-- [thats] <- readParams (...) [thises] pattern match in a do.  Which
-- resulted in my monad requiring a MonadFail constraint, a big wart
-- overall.
--
-- All the more of a wart that I'll actually need MonadFail to report
-- real failures: invalid input IntCode.  So I'm replacing my failable
-- pattern matches with a typesafe heterogeneous list of parameters
-- and eliminating *those* (which is currently: all) MonadFail
-- instances.

data a :< b = a :< b
infixr 5 :<

class Params ps where readParams :: MonadState S m => [Mode] -> m ps
instance Params () where readParams _ = return ()
instance (Param p,Params ps) => Params (p :< ps) where
  readParams   []   = error "Internal error: Ran out of modes"
  readParams (m:ms) = do p <- readParam m =<< readInstr
                         ps <- readParams ms
                         return (p :< ps)

-- Improved interfaces for some recurring themes

runAscii :: RAM -> String -> String
runAscii prg = map chr . runIntStream prg . map ord

runAscii' :: RAM -> String -> (String,[Int])
runAscii' prg = first (map chr) . partition (< 128) . runIntStream prg . map ord
