-- Day 13: Care Package
{-# LANGUAGE LambdaCase,DeriveAnyClass,FlexibleContexts #-}

import IntCode (RAM,getIntCode,getIntCodeFromFile,poke,runIntStream,IntCodeF(..),runIntT)

import Control.Concurrent
import Control.Exception         (Exception,bracket,handle,throwIO)
import Control.Monad             (forM_,forever,void)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Free  (FreeF(Pure,Free),runFreeT)
import Control.Monad.State.Lazy  (lift,evalState,get,modify')
import Data.Array.IO
import Data.Either               (lefts,rights)
import Data.IORef
import Data.Maybe                (fromMaybe)
import Data.List.Split           (chunksOf)
import System.Environment        (getArgs)
import System.Exit               (exitFailure)
import System.IO                 (hPutStrLn,stderr)
import Graphics.Vty hiding (Input,Output)

type Pos = (Int,Int)
data Tile = Empty | Wall | Block | Paddle | Ball deriving (Eq,Enum)
type TileOutput = (Pos,Tile)
newtype ScoreOutput = ScoreOutput { getScore :: Int }
type ArcadeOutput = Either TileOutput ScoreOutput
data JoystickTilt = TiltLeft | TiltNeutral | TiltRight deriving Enum
type Strategy = RAM -> [ArcadeOutput]

tiltToInt :: JoystickTilt -> Int
tiltToInt = subtract 1 . fromEnum

parseOutputs :: [Int] -> [ArcadeOutput]
parseOutputs = map readTile . chunksOf 3

readTile :: [Int] -> ArcadeOutput
readTile [-1,0,s] = Right (ScoreOutput s)
readTile [ x,y,t] = Left ((y,x),toEnum t)
readTile wtf = error $ "Error: got this “tile”: " ++ show wtf

data FollowerState = FS { ballPos :: Pos, paddlePos :: Pos }

follower :: Strategy
follower game = evalState (agent (runIntT game)) state0 where
  state0 = FS (comeon "ball") (comeon "paddle")
  comeon item = error $ "IntCode read joystick before even displaying a " ++ item
                        ++ " — gimme a break!"
  agent f = runFreeT f >>= \case
    Pure () -> return []
    Free (Input cont) -> do
      FS {ballPos = (_,bx), paddlePos = (_,px)} <- get
      agent (cont (signum (bx - px)))
    Free (Output x cont) -> bailWith "Unconforming IntCode" $ do
      Free (Output y cont')       <- lift (runFreeT cont )
      Free (Output tileId cont'') <- lift (runFreeT cont')
      let ao = readTile [x,y,tileId]
      case ao of Left (pos,Paddle) -> modify' $ \as -> as {paddlePos = pos}
                 Left (pos,Ball)   -> modify' $ \as -> as {ballPos   = pos}
                 _                 -> pure ()
      fmap (ao :) $ lift $ agent cont''
  bailWith msg = fmap (fromMaybe (error msg)) . runMaybeT

fromStream :: [JoystickTilt] -> Strategy
fromStream inputs = parseOutputs . flip runIntStream (map tiltToInt inputs)

main :: IO ()
main = do
  args <- getArgs
  (getGame,ui,strategy,chan) <- case args of
    [] -> pure (getIntCode,False,follower,Nothing)
    ["--visual",fileName] ->
      pure (getIntCodeFromFile fileName,True,follower,Nothing)
    ["--interactive",fileName] -> do
      c <- newChan :: IO (Chan JoystickTilt)
      inputs <- getChanContents c
      pure (getIntCodeFromFile fileName,True,fromStream inputs,Just c)
    _ -> do
      hPutStrLn stderr "Usage: {prg} < input"
      hPutStrLn stderr "       {prg} {--visual|--interactive} input"
      exitFailure

  game <- getGame
  let display0 = lefts $ parseOutputs $ runIntStream game []
      initialBlockCount = length $ filter ((== Block) . snd) display0
      blockReport = "Block tiles on initial screen: " ++ show initialBlockCount

  let height = 1 + maximum (map (fst . fst) display0)
      width  = 1 + maximum (map (snd . fst) display0)
      gameForFree = poke game [(0,2)]
      updateStream = strategy gameForFree

  case ui of
    False -> do print initialBlockCount
                print $ getScore $ last $ rights updateStream

    True -> bracket (mkVty =<< standardIOConfig) shutdown $
            vtyUi blockReport width height updateStream chan

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

data Quit = Quit deriving (Show,Exception)

vtyUi :: String -> Int -> Int -> [ArcadeOutput]
      -> Maybe (Chan JoystickTilt) -> Vty
      -> IO ()
vtyUi blockReport width height updateStream chan vty = do
  let infoImg = string (attr `withStyle` italic) blockReport
      instructionsImg = string attr "Use ← arrow keys → to move paddle" <->
                        string attr "      down↓arrow stays in place" <->
                        string attr "          q to quit"
      instrImg = maybe (resize (imageWidth instructionsImg)
                               (imageHeight instructionsImg) emptyImage)
                       (const instructionsImg) chan
      rightPane = instrImg <-> pad 0 2 0 2 infoImg

  raster <- newArray ((0,0),(height-1,width-1)) Empty :: IO (IOArray Pos Tile)
  score <- newIORef 0

  handle (\Quit -> pure ()) $ do

    -- If there's an open channel, translate and send keyboard events to it.
    forM_ chan $ \c -> do
      tid <- myThreadId
      forkIO $ forever $ nextEvent vty >>=
        \case EvKey KLeft  [] -> writeChan c TiltLeft
              EvKey KDown  [] -> writeChan c TiltNeutral
              EvKey KRight [] -> writeChan c TiltRight
              EvKey (KChar 'q') [] -> throwTo tid Quit
              _ -> pure ()

    -- Main thread: update display as the VM demands it
    forM_ updateStream $ \d -> do
      case d of Right (ScoreOutput s) -> writeIORef score s
                Left (pos,t) -> writeArray raster pos t
      boardImg <- vertCat . map horizCat . chunksOf width .
                  map (uncurry drawTile) <$> getAssocs raster
      scoreImg <- string (attr `withStyle` bold) .
                  ("Score: " ++ ) . show <$> readIORef score
      update vty $ picForImage $
        boardImg <|> pad 2 3 2 0 (rightPane <-> scoreImg)
      nextEventNonblocking vty >>= \case Just EvKey {} -> throwIO Quit
                                         _             -> pure ()
      
    -- After a complete run, give the user a chance to read the final score!
    void $ nextEvent vty
