{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module IntCode (getIntCode,evaluateOld,evaluate) where

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Vector (Vector,(!),(!?),(//))
import Data.List.Split (linesBy)
import Data.List (intercalate)
import Control.Arrow (second)
import Control.Monad.RWS.Lazy
import Debug.Trace

traceState = False
traceOps = False

traceOp | traceOps = trace . intercalate ","
        | otherwise = flip const

type RAM = Vector Int

getIntCode :: IO RAM
getIntCode = V.fromList . map read . linesBy (== ',') <$> getContents

evaluateOld :: RAM -> Int -> Int -> Int
evaluateOld prg i j = (! 0) $ fst $
                      execRWS (_evaluate (error "No input to evaluateOld"))
                              (R 0 0)
                              (prg // [(1,i),(2,j)])

evaluate prg i = snd $ evalRWS (_evaluate i) (R 0 0) prg

_evaluate :: [Int] -> RWS R [Int] RAM ()
_evaluate input = go input where
  -- go _ r p | traceState && traceShow (v,r,p) False = undefined
  go i = do
    p <- asks position
    op <- decodeOp <$> readAddr p

    let binOp f name = do
          (op1,dbgOp1) <- inParam op 1
          (op2,dbgOp2) <- inParam op 2
          (out,dbgOp3) <- outParam op 3
          traceOp [name,dbgOp1,dbgOp2,dbgOp3] $
            local (advancePos 4) $
            out (op1 `f` op2) *> go i

        haltOp name = traceOp [name] $ pure ()

        inOp name = do
          (out,dbgOut) <- outParam op 1
          traceOp [name,dbgOut] $
            let h:t = i in out h *> local (advancePos 2) (go t)

        outOp name = do
          (op,dbgOp) <- inParam op 1
          traceOp [name,dbgOp] $
            tell [op] *> local (advancePos 2) (go i)

        condBranchOp pr name = do
          (op1,dbgOp1) <- inParam op 1
          (op2,dbgOp2) <- inParam op 2
          traceOp [name,dbgOp1,dbgOp2] $
            local (if pr op1 then setPos op2 else advancePos 3) (go i)

        relOp name = do
          (delta,dbgDelta) <- inParam op 1
          traceOp [name,dbgDelta] $
            local (advanceRel delta) $
            local (advancePos 2) $
            go i

    case modInst op of
      1  -> binOp (+) "ADD"
      2  -> binOp (*) "MUL"
      3  -> inOp "IN"
      4  -> outOp "OUT"
      5  -> condBranchOp (/= 0) "BNZ"
      6  -> condBranchOp (== 0) "BZ"
      7  -> binOp ((fromEnum .) .  (<)) "LT"
      8  -> binOp ((fromEnum .) . (==)) "EQ"
      9  -> relOp "INCR"
      99 -> haltOp "HCF"
      x  -> error $ "Unknown opcode " ++ show x

data Op = Op { modInst :: !Int, opInst :: !Int }

decodeOp :: Int -> Op
decodeOp n = Op (n `mod` 100) (n `div` 100)

readAddr :: MonadState RAM m => Int -> m Int
readAddr a | a < 0 = error $ "Read from negative address " ++ show a
readAddr addr = fromMaybe 0 . (!? addr) <$> get

writeAddr :: MonadState RAM m => Int -> Int -> m ()
writeAddr a n | n < 0 =
  error $ "Write at negative address " ++ show (a,n)
writeAddr t n = do
  v <- get
  let v' | t < V.length v = v
         | otherwise = v <> V.replicate (t - V.length v + 1) 0
      v'' = v' // [(t,n)]
  n `seq` v'' `seq` put v''

data Mode = Position | Immediate | Relative deriving (Show,Enum)

mode :: Op -> Int -> Mode
mode op operand = toEnum $ opInst op `div` 10^(operand-1) `mod` 10

data R = R { position :: !Int, relative :: !Int }

setPos p r = r { position = p }
advancePos delta r@R{position} = r { position = position + delta }
advanceRel delta r@R{relative} = r { relative = relative + delta }

inParam :: (MonadReader R m,MonadState RAM m) => Op -> Int -> m (Int,String)
inParam op o = do
  p <- asks position
  readAddr (p+o) >>= \s -> case mode op o of
    Position  -> readAddr    s  >>= \v -> pure (v,"[" ++ show s ++ "]")
    Immediate ->                          pure (s,show s)
    Relative  -> asks relative >>= \r ->
                 readAddr (r+s) >>= \v -> pure (v,"R[" ++ show s ++ "]")

outParam :: (MonadReader R m,MonadState RAM m)
         => Op -> Int -> m (Int -> m (),String)
outParam op o = do
  p <- asks position
  readAddr (p+o) >>= \t -> case mode op o of
    Position -> pure (writeAddr      t,"[" ++ show t ++ "]")
    Relative -> asks relative >>= \r ->
                pure (writeAddr (r + t),"R[" ++ show p ++ "]")
    x -> error $ "Invalid output addressing mode " ++ show x
