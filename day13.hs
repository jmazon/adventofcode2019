import IntCode

import Control.Concurrent
import Control.Exception (Exception,bracket,handle,throwIO)
import Control.Monad (forM_,forever,void)
import Data.Array.IO
import Data.Either (lefts,rights)
import Data.IORef
import Data.List (unfoldr)
import Data.List.Split (chunksOf)
import Data.Vector ((//))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,stderr)
import Graphics.Vty hiding (Output)

type Pos = (Int,Int)
data Tile = Empty | Wall | Block | Paddle | Ball deriving (Eq,Enum)
type TileOutput = (Pos,Tile)
type ScoreOutput = Int
type Output = Either TileOutput ScoreOutput
type JoystickTilt = Int

parseOutputs :: [Int] -> [Output]
parseOutputs = map readTile . chunksOf 3
  where readTile [-1,0,s] = Right s
        readTile [ x,y,t] = Left ((y,x),toEnum t)
        readTile wtf = error $ "Error: got this \"tile\": " ++ show wtf

-- The follow strategy: remember where the paddle is; send a direction
-- event every time we see the ball.  This happens to work except for
-- the very first time the screen is displayed; hence the reverse
-- operation there to spare a *lot* of logic here.
follow :: [Output] -> [JoystickTilt]
follow s = unfoldr f (undefined,s) where
  f (px,Left ((_,bx),  Ball):stream) = Just (move px bx,(px,stream))
  f (_ ,Left ((_,px),Paddle):stream) =                f (px,stream)
  f (px,     _              :stream) =                f (px,stream)
  f (_ ,                   []      ) = Nothing
  move px bx = signum (bx - px)

main :: IO ()
main = do
  args <- getArgs
  (get,ui,strategy,chan) <-
    case args of
      [] -> pure (getIntCode,False,follow,Nothing)
      ["--visual",fileName] ->
        pure (getIntCodeFromFile fileName,True,follow,Nothing)
      ["--interactive",fileName] -> do
        c <- newChan
        inputs <- getChanContents c
        pure (getIntCodeFromFile fileName,True,const inputs,Just c)
      _ -> do
        hPutStrLn stderr "Usage: {prg} < input"
        hPutStrLn stderr "       {prg} {--visual|--interactive} input"
        exitFailure

  prg <- get
  let display0 = lefts $ parseOutputs $ evaluate prg []
      initialBlockCount = length $ filter ((== Block) . snd) display0
      blockReport = "Block tiles on initial screen: " ++ show initialBlockCount

  let height = 1 + maximum (map (fst . fst) display0)
      width  = 1 + maximum (map (snd . fst) display0)
      hack = prg // [(0,2)]

  let (initialDisplay,updateStream) = splitAt (width * height) $
                                      parseOutputs $
                                      evaluate hack controlStream
      controlStream = strategy (reverse initialDisplay ++ updateStream)

  case ui of
    False -> do putStrLn blockReport
                print $ last $ rights updateStream

    True -> bracket (mkVty =<< standardIOConfig) shutdown $
            vtyUi blockReport width height initialDisplay updateStream chan

attr :: Attr
attr = defAttr `withBackColor` brightWhite `withForeColor` black

drawTile :: Pos -> Tile -> Image
drawTile   _   Empty  = char  attr                                         ' '
drawTile   _   Wall   = char (attr `withForeColor`  white `withStyle` dim) '█'
drawTile   _   Paddle = char (attr `withForeColor`  black `withStyle` dim) '▀'
drawTile   _   Ball   = char (attr `withForeColor` yellow `withStyle` dim) '•'
drawTile (y,x) Block  = char (attr `withForeColor` c) b
  where c = [red,yellow,green,cyan,blue,magenta] !! ((x + y) `mod` 6)
        b = "░▒▓" !! (y `mod` 3)

data Quit = Quit deriving Show
instance Exception Quit

vtyUi :: String -> Int -> Int
      -> [Output] -> [Output]
      -> Maybe (Chan JoystickTilt) -> Vty
      -> IO ()
vtyUi blockReport width height initialDisplay updateStream chan vty = do
  let infoImg = translate 0 3 $ string (attr `withStyle` italic) blockReport
  raster <- newListArray ((0,0),(height-1,width-1))
                         (map snd (lefts initialDisplay))
            :: IO (IOArray (Int,Int) Tile)
  score <- newIORef 0

  handle (\Quit -> pure ()) $ do

    forM_ chan $ \c -> do
      tid <- myThreadId
      forkIO $ forever $ do
        e <- nextEvent vty
        case e of EvKey KLeft  [] -> writeChan c (-1)
                  EvKey KDown  [] -> writeChan c   0
                  EvKey KRight [] -> writeChan c   1
                  EvKey (KChar 'q') [] -> throwTo tid Quit
                  _ -> pure ()
    
    forM_ updateStream $ \d -> do
      case d of Right s -> writeIORef score s
                Left t  -> uncurry (writeArray raster) t
      boardImg <- vertCat . map horizCat . chunksOf width .
                  map (uncurry drawTile) <$> getAssocs raster
      scoreImg <- string (attr `withStyle` bold) .
                  ("Score: " ++ ) . show <$> readIORef score
      update vty $ picForImage $
        boardImg <|> pad 2 12 2 0 (scoreImg <-> infoImg)
      sequence . (throwIO Quit <$) =<< nextEventNonblocking vty
      
    void $ nextEvent vty
