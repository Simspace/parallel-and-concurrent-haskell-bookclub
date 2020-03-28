{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module BoundMVar where

import BoundedClass
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception (mask_)
import Data.Vector (Vector)
import qualified Data.Vector as V

data BoundedMVar a = BoundedMVar
  { capacity :: Int
  , readPos :: MVar Int
  , writePos :: MVar Int
  , contents :: Vector (MVar a)
  }

instance BoundedChan BoundedMVar where
    readChan = readBoundedMVar
    writeChan = writeBoundedMVar

newBoundedMVar :: Int -> IO (BoundedMVar a)
newBoundedMVar n = BoundedMVar n <$> newMVar 0 <*> newMVar 0 <*> V.replicateM n newEmptyMVar

-- | Writing locks the write index, and attempts to put the payload in the
-- mvar in that index. If that MVar is full, then write will block. A full
-- MVar indicates that the index has not yet been read.
writeBoundedMVar :: BoundedMVar a -> a -> IO ()
writeBoundedMVar c@BoundedMVar{..} a = mask_ $
    modifyMVar_ writePos $ \w -> do
      putMVar (contents V.! w) a -- blocks until empty
      pure $ nextIndex c w

-- | Reading locks the read index, and attempts to take the payload from
-- the mvar in that index. If that MVar is not full, then read will block.
-- A full MVar indicates that the index has not yet been written
readBoundedMVar :: BoundedMVar a -> IO a
readBoundedMVar c@BoundedMVar{..} = mask_ $
    modifyMVar readPos $ \r -> do
      a <- takeMVar $ contents V.! r
      pure (nextIndex c r, a)

-- | Indexes for read and write proceed "around" the circular buffer
-- the synchronization of the MVars keeps them from overlapping incorrectly
nextIndex :: BoundedMVar a -> Int -> Int
nextIndex BoundedMVar{capacity} i =
    (i + 1) `mod` capacity
