import           Data.Char
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector,(!?))
import           Data.Maybe

import System.Environment

-- Part 1: compute FFT rigorously as specified.
-- Takes about 15s in interpreted mode, 2s compiled.

-- main = interact (part1 fftReference)

fftReference :: [Int] -> [[Int]]
fftReference = iterate phase where
  phase list = zipWith (const produce) list [1..] where
    produce i = abs $ (`rem` 10) $ sum $ zipWith (*) list $
                tail $ cycle $ concatMap (replicate i) [0,1,0,-1]

readSignal = map digitToInt . head . lines
readSignalV = V.fromList . readSignal

writeSignal = unlines . pure . map intToDigit
writeSignalV = writeSignal . V.toList

part1 fft = writeSignal . take 8 . (!! 100) . fft . readSignal

-- Part 2: optimize an individual FFT phase from O(N²) to O(N×log N)
-- using partial sums to group batches of 1s and -1s together.
-- Takes about 2min compiled, but that passes.

-- main = interact part2

fft :: Vector Int -> [Vector Int]
fft = iterate phase where
  phase list = V.map produce (V.enumFromTo 1 n) where
    n = V.length list
    partials = V.scanl (+) 0 list
    p i = fromMaybe (V.last partials) $ partials !? (i-1)
    produce i = abs $ sum [ p(j*s+2*i) - p(j*s+i) - p(j*s+4*i) + p(j*s+3*i)
                          | j <- [0..(n+s-1) `div` s-1]] `rem` 10
      where s = 4 * i

part2 c = writeSignalV result where
  signal = V.concat $ replicate 10000 $ readSignalV $ head $ lines c
  offset = V.foldl1 (\a b -> 10*a+b) (V.take 7 signal)
  result = V.take 8 . V.drop offset $ fft signal !! 100

-- I wasn't smart enough to notice it before I saw it on the
-- megathread: the offset points to the second half of the signal.
-- That part is only dependent on the same part of its source, and can
-- be computed in O(N).  So here's a fast solution.
-- Runs in half a second compiled.

latterHalfFft :: Vector Int -> [Vector Int]
latterHalfFft = iterate partialize where
  partialize signal = V.map (`rem` 10) $ V.scanr1 (+) signal

smartPart2 c
  | offset < V.length signal `div` 2 =
      error "Can't be smart on first half of signal"
  | otherwise = writeSignalV result where
  signal = V.concat $ replicate 10000 $ readSignalV $ head $ lines c
  offset = V.foldl1 (\a b -> 10*a+b) (V.take 7 signal)
  result = V.take 8 $ latterHalfFft (V.drop offset signal) !! 100

main = do
  args <- getArgs
  case args of
    ["part1"] -> interact (part1 fftReference)
    ["part2"] -> interact part2
    ["smart"] -> interact smartPart2
    [] -> getContents >>= \c -> do
      putStr (part1 (map V.toList . fft . V.fromList) c)
      putStr (smartPart2 c)
    _ -> putStrLn "Usage: <prg> [part1|part2|smart] < input"
