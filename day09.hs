import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Vector (Vector,(!?),(//))
import Data.List.Split (linesBy)
import Data.List (intercalate)
import Debug.Trace

traceState = False
traceOps = False
traceOp | traceOps = trace . intercalate ","
        | otherwise = flip const

evaluate :: Vector Int -> [Int] -> [Int]
evaluate v0 input = go v0 input 0 0 where
  go v _ r p | traceState && traceShow (v,r,p) False = undefined
  go v i r p = case modInst `mod` 100 of
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

    where modInst = fromMaybe (error $ "IP at " ++ show p) (v !? p)
          mode operand = modInst `div` 10^(operand+1) `mod` 10

          inParam :: Int -> (Int,String)
          inParam o = case mode o of
              0 -> (readAddr s,"[" ++ show s ++ "]")
              1 -> (s,show s)
              2 -> (readAddr (r+s),"R[" ++ show s ++ "]")
              x -> error $ "Unknown input addressing mode " ++ show x
            where s = readAddr (p+o)

          outParam :: Int -> (Int -> Vector Int,String)
          outParam o = case mode o of
              0 -> (writeAddr      t ,"[" ++ show t ++ "]")
              2 -> (writeAddr (r + t),"R[" ++ show p ++ "]")
              x -> error $ "Unknown output addressing mode " ++ show x
            where t = readAddr (p+o)

          binOp op name = traceOp [name,dbgOp1,dbgOp2,dbgOp3] $
                          go (out (op1 `op` op2)) i r (p+4)
            where (op1,dbgOp1) = inParam 1
                  (op2,dbgOp2) = inParam 2
                  (out,dbgOp3) = outParam 3

          haltOp name = traceOp [name] []

          inOp name = traceOp [name,dbgOut] $
                      let h:t = i in go (out h) t r (p+2)
            where (out,dbgOut) = outParam 1

          outOp name = traceOp [name,dbgOp] $
                       op : go v i r (p+2)
            where (op,dbgOp) = inParam 1

          condBranchOp pr name = traceOp [name,dbgOp1,dbgOp2] $
                                 go v i r (if pr op1 then op2 else p+3)
            where (op1,dbgOp1) = inParam 1
                  (op2,dbgOp2) = inParam 2

          relOp name = traceOp [name,dbgDelta] $
                       go v i (r + delta) (p+2)
            where (delta,dbgDelta) = inParam 1

          readAddr :: Int -> Int
          readAddr a | a < 0 = error $ "Read from negative address " ++ show a
          readAddr addr = fromMaybe 0 (v !? addr)

          writeAddr :: Int -> Int -> Vector Int
          writeAddr a n | n < 0 =
            error $ "Write at negative address " ++ show (a,n)
          writeAddr t n = v' // [(t,n)] where
            v' | t < V.length v = v
               | otherwise = v <> V.replicate (t - V.length v + 1) 0

main = do
  prg <- V.fromList . map read . linesBy (== ',') <$> getContents
  print $ evaluate prg [1]
  print $ evaluate prg [2]
