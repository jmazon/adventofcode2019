-- Day 16: Flawed Frequency Transmission

import           Data.Int (Int32)
import           Data.Char
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector,(!?))
import qualified Data.Vector as LV -- lazy
import           Data.Maybe

import System.Environment

-- For the full puzzle, I measure 597ms using Int32, 651ms using 64.
-- A marginal win using the “good” code, but a slightly bigger one for
-- my original part 2.  (that I didn't really have the opportunity to
-- make use of, since its first shot succeeded!)
type Val = Int32
type FFT = Int -> Vector Val -> Vector Val

-- For part 1 I computed FFT rigorously as specified.
-- Took about 15s in interpreted mode, 2s compiled.
-- It's a lot faster than that now I migrated my lists to vectors.
fftReference :: FFT
fftReference u = (!! u) . iterate phase where
  phase list = V.generate n produce where
    n = V.length list
    produce i = abs $ (`rem` 10) $ V.sum $ V.zipWith (*) list $
                V.tail $ V.concat $ replicate ((n + 4*i-1) `div` (4*i)) $ V.concatMap (V.replicate (i+1)) (V.fromList [0,1,0,-1])

readSignalV :: String -> Vector Val
readSignalV = V.fromList . map (fromIntegral . digitToInt) . head . lines

writeSignalV :: Vector Val -> String
writeSignalV = unlines . pure . map (intToDigit . fromIntegral) . V.toList

-- Part 1 asks for the first 8 digits of fft^100 of input signal
part1 :: FFT -> String -> String
part1 fft = writeSignalV . V.take 8 . fft 100 . readSignalV

-- For part 2 I optimized an individual FFT phase from O(N²) to
-- O(N×log N) using partial sums to group batches of 1s and -1s
-- together.  Took about 3'30 compiled, but that passed.  It could
-- probably be optimized a lot more using in-place vector rewriting,
-- but that's a dead-end for now.
-- Changing to Int32 brought it down to 3'15.
fftLinearithmic :: FFT
fftLinearithmic u = (!! u) . iterate phase where
  phase list = V.map produce (V.enumFromTo 1 n) where
    n = V.length list
    partials = V.scanr (+) 0 list
    p i = fromMaybe (V.last partials) $ partials !? (i-1)
    produce i = abs $ sum [ p(j*s+i) - p(j*s+2*i) - p(j*s+3*i) + p(j*s+4*i)
                          | j <- [0..(n+s-1) `div` s-1]] `rem` 10
      where s = 4 * i

-- Part 2 asks for substring(fft^100 (10000×signal),prefix(signal,7),8)
part2 :: FFT -> String -> String
part2 fft c = writeSignalV result where
  signal = V.concat $ replicate 10000 $ readSignalV $ head $ lines c
  offset = fromIntegral $ V.foldl1 (\a b -> 10*a+b) (V.take 7 signal)
  result = V.take 8 . V.drop offset $ fft 100 signal

-- I wasn't smart enough to notice it before I saw it on the
-- megathread: the offset points to the second half of the signal.
-- That part is only dependent on the same part of its source, and can
-- be computed in O(N).  So here's a fast solution.
-- Runs in half a second compiled.
latterHalfFft :: FFT
latterHalfFft u = (!! u) . iterate partialize where
  partialize signal = V.map (`rem` 10) $ V.scanr1 (+) signal

smartPart2 :: String -> String
smartPart2 c | offset < V.length signal `div` 2 =
                 error "Can't be smart on the first half of the signal"
             | otherwise = writeSignalV result where
  signal = V.concat $ replicate 10000 $ readSignalV $ head $ lines c
  offset = fromIntegral $ V.foldl1 (\a b -> 10*a+b) $ V.take 7 signal
  result = V.take 8 $ latterHalfFft 100 $ V.drop offset signal

-- But that bails if the signal starts with too low of an offset.
-- Let's try to have the best of both worlds: redefine the O(N×log N)
-- algorithm so that latter half only depends on the latter half of
-- the previous signal.  This way, with lazy evaluation, we'll only
-- fail over to the semislow process if needed, and keep the linear
-- solution where available.  With a single algorithm definition!
fftLL :: FFT
fftLL u = V.convert . (!! u) . iterate phase . LV.convert where
  phase list = LV.map produce (LV.enumFromTo 1 n) where
    n = LV.length list
    partials = LV.scanl (+) 0 list
    p i = fromMaybe (LV.last partials) $ partials LV.!? (i-1)
    produce i = abs $ sum [ p(j*s+2*i) - p(j*s+i) - p(j*s+4*i) + p(j*s+3*i)
                          | j <- [0..(n+s-1) `div` s-1]] `rem` 10
      where s = 4 * i
-- Except lazyness has a pretty big cost in thunks.  Looks like I *am*
-- going to have to bite the bullet and reimplement all of those as a
-- whole "puzzle" function that solves
-- puzzle(S,R,O) = substring(fft^100 (R×S),O,8)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> interact (part1 fftReference)
    ["part2"] -> interact (part2 fftLinearithmic)
    ["smart"] -> interact smartPart2
    ["ante"] -> do c <- getContents
                   putStr $ smartPart2 c
                   putStr $ part2 fftLL c
    [] -> do c <- getContents
             putStr (part1 fftLinearithmic c)
             putStr (smartPart2 c)
    _ -> putStrLn "Usage: <prg> [part1|part2|smart] < input"
