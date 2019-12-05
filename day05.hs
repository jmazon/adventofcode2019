import qualified Data.Vector as V
import Data.Vector (Vector,(!),(//))
import Data.List.Split (linesBy)

evaluate :: Vector Int -> [Int] -> [Int]
evaluate v input = go v input 0 where
  go v i p = case opcode of
      1 -> go (out 3 (inParam 1 + inParam 2)) i (p+4)
      2 -> go (out 3 (inParam 1 * inParam 2)) i (p+4)
      3 -> let (h:t) = i in go (out 1 h) t (p+2)
      4 -> inParam 1 : go v i (p+2)
      5 -> go v i (if inParam 1 /= 0 then inParam 2 else p+3)
      6 -> go v i (if inParam 1 == 0 then inParam 2 else p+3)
      7 -> go (out 3 $ fromEnum $ inParam 1  < inParam 2) i (p+4)
      8 -> go (out 3 $ fromEnum $ inParam 1 == inParam 2) i (p+4)
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

main = do
  prg <- V.fromList . map read . linesBy (== ',') <$> getContents
  print $ evaluate prg [1]
  print $ evaluate prg [5]
