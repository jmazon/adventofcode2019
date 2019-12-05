import qualified Data.Vector as V
import Data.Vector (Vector,(!),(//))
import Data.List.Split (linesBy)

evaluate :: Vector Int -> [Int] -> [Int]
evaluate v input = go v input 0 where
  go v i p = case modInst `mod` 100 of
      1 -> binOp (+)
      2 -> binOp (*)
      3 -> let h:t = i in go (out 1 h) t (p+2)
      4 -> inParam 1 : go v i (p+2)
      5 -> condBranchOp (/= 0)
      6 -> condBranchOp (== 0)
      7 -> binOp ((fromEnum .) .  (<))
      8 -> binOp ((fromEnum .) . (==))
      99 -> []
    where modInst = v!p
          mode i = modInst `div` 10^(i+1) `mod` 10
          decodeParam 0 p = v!p
          decodeParam 1 p = p
          inParam i = decodeParam (mode i) (v!(p+i))
          out i n | mode i == 0 = v // [(v!(p+i),n)]
          binOp op = go (out 3 (inParam 1 `op` inParam 2)) i (p+4)
          condBranchOp pr = go v i (if pr (inParam 1) then inParam 2 else p+3)

main = do
  prg <- V.fromList . map read . linesBy (== ',') <$> getContents
  print $ evaluate prg [1]
  print $ evaluate prg [5]
