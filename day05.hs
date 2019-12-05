import qualified Data.Vector as V
import Data.Vector (Vector,(!),(//))
import Data.List.Split (linesBy)

evaluate :: Vector Int -> [Int] -> [Int]
evaluate v input = go v input 0 where
  go v i p = case opcode of
      1 -> go (write v (v!(p+3)) (param mode1 (v!(p+1)) + param mode2 (v!(p+2))))
              i (p+4)
      2 -> go (write v (v!(p+3)) (param mode1 (v!(p+1)) * param mode2 (v!(p+2))))
              i (p+4)
      3 -> let (h:t) = i in go (v // [(v!(p+1),h)]) t (p+2)
      4 -> param mode1 (v!(p+1)) : go v i (p+2)
      5 -> go v i (if param mode1 (v!(p+1)) /= 0 then param mode2 (v!(p+2))
                   else p+3)
      6 -> go v i (if param mode1 (v!(p+1)) == 0 then param mode2 (v!(p+2))
                   else p+3)
      7 -> go (write v (v!(p+3)) $ fromEnum $
               param mode1 (v!(p+1)) < param mode2 (v!(p+2))) i (p+4)
      8 -> go (write v (v!(p+3)) $ fromEnum $
               param mode1 (v!(p+1)) == param mode2 (v!(p+2))) i (p+4)
      99 -> []
    where modInst = v!p
          mode1 = modInst `div` 100 `mod` 10
          mode2 = modInst `div` 1000 `mod` 10
          mode3 = modInst `div` 10000
          opcode = modInst `mod` 100
          param 0 p = v!p
          param 1 p = p
  write v p n = v // [(p,n)]

main = do
  prg <- V.fromList . map read . linesBy (== ',') <$> getContents
  print $ evaluate prg [1]
  print $ evaluate prg [5]
