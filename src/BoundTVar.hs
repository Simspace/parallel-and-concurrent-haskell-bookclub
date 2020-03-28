{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module BoundTVar where

import BoundedClass
import Control.Concurrent.STM
  (STM, atomically, TVar, newTVarIO, readTVar, writeTVar, retry)
import Data.Vector (Vector)
import qualified Data.Vector as V

data BoundedTVar a = BoundedTVar
  { capacity :: Int
  , readPos :: TVar Int
  , writePos :: TVar Int
  , contents :: Vector (TVar (Maybe a))
  }

instance BoundedChan BoundedTVar where
    readChan = atomically . readBoundedTVar
    writeChan c a = atomically $ writeBoundedTVar c a

newBoundedTVar :: Int -> IO (BoundedTVar a)
newBoundedTVar n = BoundedTVar n <$> newTVarIO 0 <*> newTVarIO 0 <*> V.replicateM n (newTVarIO Nothing)

writeBoundedTVar :: BoundedTVar a -> a -> STM ()
writeBoundedTVar c@BoundedTVar{..} a = do
    w <- readTVar writePos
    v <- readTVar (contents V.! w)
    case v of
      Just _ -> retry
      Nothing -> writeTVar (contents V.! w) (Just a)
    writeTVar writePos (nextIndex c w)

readBoundedTVar :: BoundedTVar a -> STM a
readBoundedTVar c@BoundedTVar{..} = do
    r <- readTVar readPos
    a <- readTVar (contents V.! r)
    a' <- case a of
      Just a -> pure a
      Nothing -> retry
    writeTVar (contents V.! r) Nothing
    writeTVar readPos (nextIndex c r)
    pure a'

nextIndex :: BoundedTVar a -> Int -> Int
nextIndex BoundedTVar{capacity} i =
    (i + 1) `mod` capacity
