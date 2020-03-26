module BoundedQueueSpec where

import Control.Exception.Base (AsyncException(..))
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.List (partition)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Bound
import qualified Data.Set as Set
import Control.Concurrent (forkIO, throwTo, newMVar, modifyMVar_, takeMVar)

tests :: TestTree
tests =
    testGroup "Bounded Queue"
      [ testProperty "sent and received are the same" sentAndReceived
      , testProperty "partitioned order" partitionedOrder
      , testProperty "exception safety" exceptionSafe
      ]

sentAndReceived :: [Int] -> [Int] -> Property
sentAndReceived xs ys = monadicIO $ do
  chan <- liftIO $ newBoundedChan 8
  liftIO $ forkIO $ forM_ xs (writeBoundedChan chan)
  liftIO $ forkIO $ forM_ ys (writeBoundedChan chan)
  zs <- traverse (\_ -> liftIO $ readBoundedChan chan) (xs ++ ys)
  assert $ Set.fromList zs == Set.fromList (xs ++ ys)

partitionedOrder :: [Int] -> [Int] -> Property
partitionedOrder xs ys = monadicIO $ do
  chan <- liftIO $ newBoundedChan 8
  liftIO $ forkIO $ forM_ xs (\x -> writeBoundedChan chan (1, x))
  liftIO $ forkIO $ forM_ ys (\y -> writeBoundedChan chan (2, y))
  zs <- traverse (\_ -> liftIO $ readBoundedChan chan) (xs ++ ys)
  let (xs', ys') = partition ((1 ==) . fst) zs
  assert $ (snd <$> xs') == xs
  assert $ (snd <$> ys') == ys

exceptionSafe :: [Int] -> Property
exceptionSafe xs = monadicIO $ do
  chan <- liftIO $ newBoundedChan 8
  -- insert and read forever so they're still doing stuff when
  -- unceremoniously killed
  wThread <- liftIO $ forkIO $ forM_ (cycle xs) (writeBoundedChan chan)
  rThread <- liftIO $ forkIO $ forever readBoundedChan chan
  -- unceremoniously kill the threds
  liftIO $ throwTo wThread ThreadKilled
  liftIO $ throwTo rThread ThreadKilled

  -- write everything, and then read it. Timeout will hit probably after
  -- the write is exhausted
  liftIO $ forkIO $ forM_ xs (writeBoundedChan chan)
  zs <- liftIO $ newMVar []
  liftIO $ timeout (1 * 10 ^ 6) $ forever $ do
    z <- readBoundedChan chan
    modifyMVar_ zs $ \zs' -> pure $ z:zs'
  zs' <- liftIO $ takeMVar zs
  assert $ Set.fromList zs' == Set.fromList xs
