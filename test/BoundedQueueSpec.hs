module BoundedQueueSpec where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (partition)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Bound
import qualified Data.Set as Set
import Control.Concurrent (forkIO)

tests :: TestTree
tests =
    testGroup "Bounded Queue"
      [ testProperty "sent and received are the same" sentAndReceived
      , testProperty "partitioned order" partitionedOrder
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
