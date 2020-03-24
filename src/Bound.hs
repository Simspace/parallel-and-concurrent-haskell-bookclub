{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Bound where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Vector (Vector)
import qualified Data.Vector as V

data BoundedChan a = BoundedChan
  { capacity :: Int
  , readPos :: MVar Int
  , writePos :: MVar Int
  , contents :: Vector (MVar a)
  }

newBoundedChan :: Int -> IO (BoundedChan a)
newBoundedChan n = BoundedChan n <$> newMVar 0 <*> newMVar 0 <*> V.replicateM n newEmptyMVar

-- | Writing locks the write index, and attempts to put the payload in the
-- mvar in that index. If that MVar is full, then write will block. A full
-- MVar indicates that the index has not yet been read.
writeBoundedChan :: BoundedChan a -> a -> IO ()
writeBoundedChan c@BoundedChan{..} a = do
      w <- takeMVar writePos
      putMVar (contents V.! w) a -- blocks until empty
      putMVar writePos (nextIndex c w)

-- | Reading locks the read index, and attempts to take the payload from
-- the mvar in that index. If that MVar is not full, then read will block.
-- A full MVar indicates that the index has not yet been written
readBoundedChan :: BoundedChan a -> IO a
readBoundedChan c@BoundedChan{..} = do
    r <- takeMVar readPos
    a <- takeMVar $ contents V.! r
    putMVar readPos (nextIndex c r)
    pure a

-- | Indexes for read and write proceed "around" the circular buffer
-- the synchronization of the MVars keeps them from overlapping incorrectly
nextIndex :: BoundedChan a -> Int -> Int
nextIndex BoundedChan{capacity} i =
    (i + 1) `mod` capacity
