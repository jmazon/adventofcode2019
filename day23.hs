{-# LANGUAGE FlexibleContexts, RecursiveDo, LambdaCase #-}
import           IntCode
import qualified Data.IntMap.Strict as M
import           Data.Bits (bit,setBit,clearBit)
import           Data.IORef
import           Data.List.Split (chunksOf)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Trans.Free
import           Control.Monad.State.Strict
import           Control.Monad.Reader

runNIC :: (Int -> (Int,Int) -> IO ()) -> TVar Int -> TQueue Int
       -> Free IntCodeF () -> Int -> IO ()
runNIC sendTo sem chan f self = runReaderT (evalStateT (iterM go f) []) False where
  go (Input cont) = do
    idle <- ask
    (idle',v) <- liftIO $ atomically (tryReadTQueue chan) >>=
      \case Nothing -> do
              when idle $ atomically $ modifyTVar' sem (`clearBit` self)
              yield
              pure (True,-1)
            Just v -> do
              atomically $ modifyTVar' sem (`setBit` self)
              pure (False,Val v)
    local (const idle') (cont v)
  go (Output v cont) = do
    liftIO $ atomically $ modifyTVar' sem (`setBit` self)
    get >>= \case [x,target] -> do
                    let y = unVal v
                    liftIO $ sendTo target (x,y)
                    put []
                  vs -> put (unVal v:vs)
    local (const False) cont

runNAT :: (Int -> (Int,Int) -> IO ()) -> TVar Int -> TQueue Int
       -> ((Int,Int) -> IO ()) -> (Int -> IO ()) -> IO ()
runNAT sendTo sem q onUpdate onResume = do
  buf <- newIORef (error "NAT before idle")
  forever $ do
    atomically $ do
      n <- readTVar sem
      check (n == 0)
    flush <- atomically (flushTQueue q)
    forM_ (chunksOf 2 flush) $ \[x,y] -> writeIORef buf (x,y) *> onUpdate (x,y)
    (x,y) <- readIORef buf
    sendTo 0 (x,y)
    onResume y
    yield

main :: IO ()
main = do
  prg <- getIntCode
  sem <- newTVarIO (bit 50 - 1)
  natPackets <- newEmptyMVar
  natStream <- newChan
  rec chans <- fmap M.fromList $ do

        let sendTo nic (x,y) = atomically $ do
              let q = chans M.! nic
              writeTQueue q x
              writeTQueue q y

        nics <- forM [0..49] $ \i -> do
          c <- newTQueueIO
          atomically (writeTQueue c i)
          void $ forkIO $ runNIC sendTo sem c (evaluateF prg) i
          return (i,c)

        nat <- do
          c <- newTQueueIO
          void $ forkIO $ runNAT sendTo sem c (void . tryPutMVar natPackets)
                                              (writeChan natStream)
          return (255,c)

        pure (nat : nics)

  print . snd =<< takeMVar natPackets
  print . extractDupe =<< getChanContents natStream

extractDupe :: (Show a,Eq a) => [a] -> a
extractDupe (a:xs@(b:_)) | a == b = a
                         | otherwise = extractDupe xs
extractDupe _ = error "extractDupe: exhausted input"
