import qualified Data.Vector as V
import Data.Vector (Vector,(!),(//))
import Data.List.Split (linesBy)

evaluate :: Vector Int -> [Int] -> [Int]
evaluate v input = go v input 0 where
  go v i p = case opcode of
      1 -> binOp (+)
      2 -> binOp (*)
      3 -> let (h:t) = i in go (out 1 h) t (p+2)
      4 -> inParam 1 : go v i (p+2)
      5 -> testOp (/= 0)
      6 -> testOp (== 0)
      7 -> binOp ((fromEnum .) .  (<))
      8 -> binOp ((fromEnum .) . (==))
      99 -> []
    where modInst = v!p
          mode 1 = modInst `div` 100 `mod` 10
          mode 2 = modInst `div` 1000 `mod` 10
          mode 3 = modInst `div` 10000
          opcode = modInst `mod` 100
          param 0 p = v!p
          param 1 p = p
          inParam i = param (mode i) (v!(p+i))
          out i n | mode i == 0 = v // [(v!(p+i),n)]
          binOp o = go (out 3 (inParam 1 `o` inParam 2)) i (p+4)
          testOp pr = go v i (if pr (inParam 1) then inParam 2 else p+3)

main = do
  prg <- V.fromList . map read . linesBy (== ',') <$> getContents
  print $ evaluate prg [1]
  print $ evaluate prg [5]
