{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Bound where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Vector

data BoundedChan a = BoundedChan
  { capacity :: Int
  , readPos :: MVar Int
  , writePos :: MVar Int
  , contents :: Vector (MVar a)
  }

newBoundedChan :: Int -> IO (BoundedChan a)
newBoundedChan n = BoundedChan n <$> newMVar 0 <*> newMVar 0 <*> replicateM n newEmptyMVar

writeBoundedChan :: BoundedChan a -> a -> IO ()
writeBoundedChan BoundedChan{..} a = do
  r <- takeMVar readPos
  w <- takeMVar writePos
  modifyMVar writePos \w -> 


readBoundedChan :: BoundedChan a -> IO a
readBoundedChan BoundedChan{..} = _
