{-# LANGUAGE BangPatterns #-}

module FindFile where

import Async
import Control.Concurrent (MVar, newMVar, modifyMVar)
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Data.List (sort)
import System.Directory (listDirectory, doesDirectoryExist)

newtype NBSem = NBSem (IORef Int)

newNBSem :: Int -> IO NBSem
newNBSem i = NBSem <$> newIORef i

tryAcquireNBSem :: NBSem -> IO Bool
tryAcquireNBSem (NBSem m) =
    atomicModifyIORef m $ \i ->
      if i == 0
        then (i, False)
        else let !z = i - 1 in (z, True)

releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) =
    atomicModifyIORef m $ \i ->
    let !z = i + 1 in (z, ())

find :: NBSem -> String -> FilePath -> IO (Maybe FilePath)
find sem s d = do
    fs <- sort <$> listDirectory d
    if any (== s) fs
      then pure (Just (d <> s))
      else do
        let ps = (d <>) <$> fs
        (foldr (subfind sem s) dowait ps) []
  where dowait :: [Async (Maybe a)] -> IO (Maybe a)
        dowait as = loop (reverse as)
        loop :: [Async (Maybe a)] -> IO (Maybe a)
        loop [] = pure Nothing
        loop (a:as) = do
          r <- wait a
          case r of
            Nothing -> loop as
            Just a -> pure $ Just a

subfind ::
  NBSem
  -> String
  -> FilePath
  -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
  -> [Async (Maybe FilePath)]
  -> IO (Maybe FilePath)
subfind sem s p inner asyncs = do
    isdir <- doesDirectoryExist p
    if not isdir
      then inner asyncs
      else do
        q <- tryAcquireNBSem sem
        if q
          then do
            let dofind = find sem s p `finally` releaseNBSem sem
            withAsync dofind $ \a -> inner (a:asyncs)
          else do
            r <- find sem s p
            case r of
              Nothing -> inner asyncs
              Just _ -> pure r
