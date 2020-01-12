-- Day 23: Category Six
{-# LANGUAGE FlexibleContexts,RecursiveDo,LambdaCase #-}

import IntCode (getIntCode,runIntF,IntCodeM,GenCodeF(..))

import Data.IntMap.Strict (fromList,(!))
import Data.Bits          (bit,setBit,clearBit)
import Data.IORef
import Data.List.Split    (chunksOf)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.Free
import Control.Monad.State.Strict
import Control.Monad.Reader

type Address = Int
type Packet = (Int,Int)
type SendAction = Address -> Packet -> IO ()
type NetKit = (SendAction,TVar Int,TQueue Int)

-- NIC integration in the network.  Implemented as a monadic
-- interpreter for the IntCode's free monad API.
--
-- Reader: Bool remembering whether idle.  I use this because of the
-- unclear definition of the network idleness: how many unsuccessful
-- input attempts are needed to consider a computer "continuously"
-- idle?  I only flag the instance as idle in the global semaphore if
-- it already failed receiving once (which puts the threshold at 2
-- failed attempts).  But I consider this a weak spot in the
-- statement: 1 could work (but yielded unstable results when I tried
-- it) and 0 arguably could too (having an empty queue and trying to
-- receive, read strictly, doesn't necessarily mean the attempt to
-- read should fail a single time, if the NAT acts fast enough).
--
-- State: temporary storage for the address and X value until we get a
-- complete packet to send.
runNIC :: NetKit -> IntCodeM () -> Int -> IO ()
runNIC (sendTo,sem,chan) f self = runReaderT (evalStateT (iterM go f) []) False
  where go (Input cont) = do
          idle <- ask
          (idle',v) <- liftIO $ atomically (tryReadTQueue chan) >>= \case
            Nothing -> do            -- input requested, queue empty
              when idle $ atomically $ modifyTVar' sem (`clearBit` self)
              yield -- return at least one -1 before declaring instance idle
              pure (True,-1)
            Just v -> do             -- input requested, queue non-empty
              atomically $ modifyTVar' sem (`setBit` self)
              pure (False,v)
          local (const idle') (cont v)
        go (Output v cont) = do
          liftIO $ atomically $ modifyTVar' sem (`setBit` self)
          get >>= \case [x,target] -> do       -- output, buffer full
                          let y = v
                          liftIO $ sendTo target (x,y)
                          put []
                        vs -> put (v:vs) -- output, buffer not full
          local (const False) cont

-- NAT integration to the network and implementation.
--
-- To keep it single-threaded, I kind of cheated here and don't
-- actually remember the last packet received: instead I let them
-- queue up and only "update" (well, take the last) when I already
-- detected the network as idle.
--
-- The two hooks are to report first incoming and all outgoing packets
-- to the main thread for analysis (and global termination).
runNAT :: NetKit -> (Packet -> IO ()) -> (Int -> IO ()) -> IO ()
runNAT (sendTo,sem,q) onUpdate onResume = do
  buf <- newIORef (error "NAT before idle") -- let it never be evaluated
  forever $ do atomically $ readTVar sem >>= check . (== 0)
               flush <- atomically (flushTQueue q)
               forM_ (chunksOf 2 flush) $ \[x,y] ->
                 writeIORef buf (x,y) *> onUpdate (x,y)
               (x,y) <- readIORef buf
               sendTo 0 (x,y)
               onResume y
               yield

main :: IO ()
main = do
  prg <- getIntCode

  sem <- newTVarIO (bit 50 - 1) -- crude semaphore bitset to detect idleness
  natPackets <- newEmptyMVar    -- part 1: storage point for first packet to NAT
  natStream <- newChan          -- part 2: stream of Ys sent to address 0

  -- I do love it when I get an actual opportunity to use recursive do!
  rec chans <- fmap fromList $ do

        let sendTo address (x,y) = atomically $ do writeTQueue q x
                                                   writeTQueue q y
              where q = chans ! address

        nics <- forM [0..49] $ \i -> do
          c <- newTQueueIO
          atomically (writeTQueue c i)
          void $ forkIO $ runNIC (sendTo,sem,c) (runIntF prg) i
          return (i,c)

        nat <- do
          c <- newTQueueIO
          void $ forkIO $ runNAT (sendTo,sem,c) (void . tryPutMVar natPackets)
                                                (writeChan natStream)
          return (255,c)

        pure (nat : nics)

  print . snd =<< takeMVar natPackets
  print . extractDupe =<< getChanContents natStream

extractDupe :: Eq a => [a] -> a
extractDupe (a:xs@(b:_)) | b == a = a
                         | otherwise = extractDupe xs
extractDupe _ = error "extractDupe: exhausted input"
