{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
import IntCode
import qualified Data.IntMap.Strict as M
import Data.Bits
import Data.List.Split (chunksOf)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.Free
import Control.Monad.State.Strict
import Control.Monad.Extra

import Debug.Trace

runNIC :: M.IntMap (TQueue Int) -> TVar Int
       -> Free IntCodeF ()
       -> (Int,TQueue Int) -> IO ()
runNIC chans sem prg (self,chan) = evalStateT (iterM go prg) Nothing where
  go (Input f) = do
    v <- liftIO $ atomically $ do
      mv <- tryReadTQueue chan
      case mv of
        Nothing -> modifyTVar' sem (`clearBit` self) *> pure (-1)
        Just v -> modifyTVar' sem (`setBit` self) *> pure (Val v)
    f v
  go (Output v f) = do
    liftIO $ atomically $ modifyTVar' sem (`setBit` self)
    s <- get
    case s of
      Nothing -> put (Just (unVal v,Nothing))
      Just (target,Nothing) -> put (Just (target,Just (unVal v)))
      Just (target,Just x) -> do
        let y = unVal v
        let q = chans M.! target
        liftIO $ atomically $ do
          writeTQueue q x
          writeTQueue q y
        put Nothing
    f

main :: IO ()
main = do
  prg <- getIntCode
  chans <- fmap M.fromList $ forM (255 : [0..49]) $ \i -> do
    c <- newTQueueIO
    atomically (writeTQueue c i)
    return (i,c)
  
  sem <- newTVarIO (2^(50 :: Int) - 1)
  
  forM_ [0..49] $ \i -> do
    forkIO $ runNIC chans sem (evaluateF prg) (i,chans M.! i)

  nat <- newChan
  void $ forkIO $ do
    let q = chans M.! 255
    255 <- atomically (readTQueue q)

    buf <- newTVarIO Nothing

    forever $ do
      y <- atomically $ do
        n <- readTVar sem
        check (n == 0)
        flush <- flushTQueue q
        forM_ (chunksOf 2 flush) $ \[x,y] -> writeTVar buf (Just (x,y))
        b <- readTVar buf
        (x,y) <- case b of Nothing -> traceM "idle before NAT" *> retry
                           Just x -> pure x
        writeTQueue (chans M.! 0) x
        writeTQueue (chans M.! 0) y
        return y
      writeChan nat y
      whileM ((== 0) <$> readTVarIO sem)

  print . extractDupe =<< getChanContents nat

extractDupe :: (Show a,Eq a) => [a] -> a
extractDupe (a:xs@(b:_)) | traceShowId a == b = a
                         | otherwise = extractDupe xs
extractDupe _ = error "extractDupe: exhausted input"
