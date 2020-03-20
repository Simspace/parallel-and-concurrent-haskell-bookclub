{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Bound where

import Control.Concurrent hiding (Chan(..))
-- import Control.Concurrent.Chan
import Data.Vector

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

data Chan a = Chan
  { readVar  :: (MVar (Stream a))
  , writeVar :: (MVar (Stream a))
  }

newChan :: IO (Chan a)
newChan = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return (Chan readVar writeVar)

readChan :: Chan a -> IO a
readChan Chan{..} = do
  stream <- takeMVar readVar
  Item val tail <- takeMVar stream
  putMVar readVar tail
  return val

writeChan :: Chan a -> a -> IO ()
writeChan Chan{..} val = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar oldHole (Item val newHole)
  putMVar writeVar newHole

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
  i <- takeMVar writePos
  putMVar (contents ! i) a
  putMVar writePos $ (i + 1) `mod` capacity

readBoundedChan :: BoundedChan a -> IO a
readBoundedChan BoundedChan{..} = do
  i <- takeMVar readPos
  x <- takeMVar $ contents ! i
  putMVar readPos $ (i + 1) `mod` capacity
  pure x
