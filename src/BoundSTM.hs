{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module BoundSTM where

import BoundedClass
import Control.Concurrent.STM
  (TMVar, newTMVarIO, newEmptyTMVarIO, STM, takeTMVar, putTMVar, atomically)
import Data.Vector (Vector)
import qualified Data.Vector as V

data BoundedSTM a = BoundedSTM
  { capacity :: Int
  , readPos :: TMVar Int
  , writePos :: TMVar Int
  , contents :: Vector (TMVar a)
  }

instance BoundedChan BoundedSTM where
    readChan = atomically . readBoundedSTM
    writeChan c a = atomically $ writeBoundedSTM c a

newBoundedSTM :: Int -> IO (BoundedSTM a)
newBoundedSTM n = BoundedSTM n <$> newTMVarIO 0 <*> newTMVarIO 0 <*> V.replicateM n newEmptyTMVarIO

writeBoundedSTM :: BoundedSTM a -> a -> STM ()
writeBoundedSTM c@BoundedSTM{..} a = do
    w <- takeTMVar writePos
    putTMVar (contents V.! w) a
    putTMVar writePos (nextIndex c w)

readBoundedSTM :: BoundedSTM a -> STM a
readBoundedSTM c@BoundedSTM{..} = do
    r <- takeTMVar readPos
    v <- takeTMVar (contents V.! r)
    putTMVar readPos (nextIndex c r)
    pure v

nextIndex :: BoundedSTM a -> Int -> Int
nextIndex BoundedSTM{capacity} i =
    (i + 1) `mod` capacity
