import IntCode

import           Data.Complex
import           Data.Function ((&),on)
import           Data.Either   (partitionEithers)
import qualified Data.Map as M
import           Control.Applicative (liftA2)
import           Control.Arrow       ((***),(&&&))
import           Control.Monad       (forM_)

i = 0 :+ 1
instance Ord n => Ord (Complex n) where
  compare = liftA2 (<>) (compare `on` realPart) (compare `on` imagPart)

robot prg startColor = result where
  commandStream = evaluate prg cameraStream
  (cameraStream,~[result]) = partitionEithers $
                             go (M.singleton 0 startColor) 0 i commandStream
  go p r d os = Left (fromEnum (M.findWithDefault False r p)) :
                case os of
                  (c:t:os') ->
                    let d' = d * if toEnum t then (-i) else i
                    in go (M.insert r (toEnum c) p) (r+d') d' os'
                  [] -> [Right p]
main = do
  prg <- getIntCode
  let part1 = robot prg False
      part2 = robot prg True
      ((x1,x2),(y1,y2)) = M.keys part2 & map (realPart &&& imagPart) &
                          unzip & minMax *** minMax
  print $ M.size part1
  forM_ [-y2 .. -y1] $ \y -> do
    forM_ [x1..x2] $ \x ->
      putChar $ if M.findWithDefault False (x :+ (-y)) part2 then '*' else ' '
    putChar '\n'
  
minMax = (,) <$> minimum <*> maximum

