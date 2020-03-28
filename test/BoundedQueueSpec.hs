module BoundedQueueSpec where

import Control.Exception.Base (AsyncException(..))
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (partition)
import System.IO (hFlush, stdout)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import BoundMVar (newBoundedMVar)
import BoundSTM (newBoundedSTM)
import BoundedClass
import qualified Data.Set as Set
import Control.Concurrent (forkIO, throwTo, newMVar, modifyMVar_, takeMVar)

tests :: TestTree
tests =
    testGroup "Bounded Queue"
      [ testProperty "sent and received are the same" $ sentAndReceived $ newBoundedMVar
      , testProperty "partitioned order" $ partitionedOrder $ newBoundedMVar
      -- testProperty "exception safety" $ exceptionSafe $ newBoundedMVar
      , testProperty "sent and received are the same" $ sentAndReceived $ newBoundedSTM
      , testProperty "partitioned order" $ partitionedOrder $ newBoundedSTM
      -- testProperty "exception safety" $ exceptionSafe $ newBoundedMVar
      ]

sentAndReceived :: (BoundedChan b) => (Int -> IO (b Int)) -> [Int] -> [Int] -> Property
sentAndReceived mkChan xs ys = monadicIO $ do
  chan <- liftIO $ mkChan 8
  liftIO $ forkIO $ forM_ xs (writeChan chan)
  liftIO $ forkIO $ forM_ ys (writeChan chan)
  zs <- traverse (\_ -> liftIO $ readChan chan) (xs ++ ys)
  assert $ Set.fromList zs == Set.fromList (xs ++ ys)

partitionedOrder :: (BoundedChan b) => (Int -> IO (b (Int, Int))) -> [Int] -> [Int] -> Property
partitionedOrder mkChan xs ys = monadicIO $ do
  chan <- liftIO $ mkChan 8
  liftIO $ forkIO $ forM_ xs (\x -> writeChan chan (1, x))
  liftIO $ forkIO $ forM_ ys (\y -> writeChan chan (2, y))
  zs <- traverse (\_ -> liftIO $ readChan chan) (xs ++ ys)
  let (xs', ys') = partition ((1 ==) . fst) zs
  assert $ (snd <$> xs') == xs
  assert $ (snd <$> ys') == ys

exceptionSafe :: (BoundedChan b) => (Int -> IO (b Int)) -> [Int] -> Property
exceptionSafe mkChan xs' = monadicIO $ do
  let xs = 1 : xs'
  chan <- liftIO $ mkChan 8
  liftIO $ putStrLn "made channel"
  liftIO $ hFlush stdout
  -- insert and readChan forever so they're still doing stuff when
  -- unceremoniously killed

  wThread <- liftIO $ forkIO $ forM_ (cycle xs) (writeChan chan)
  liftIO $ putStrLn "writing"
  liftIO $ hFlush stdout

  rThread <- liftIO $ forkIO $ forever $ readChan chan
  liftIO $ putStrLn "reading"
  liftIO $ hFlush stdout

  -- unceremoniously kill the threds
  liftIO $ throwTo wThread ThreadKilled
  liftIO $ throwTo rThread ThreadKilled
  liftIO $ putStrLn "Threads killed"
  liftIO $ hFlush stdout

  -- writeChan everything, and then readChan it. Timeout will hit probably after
  -- the writeChan is exhausted
  liftIO $ forkIO $ forM_ xs $ \x -> do
    (writeChan chan x)
    putStrLn $ "wrote " <> show x
    hFlush stdout
  zs <- liftIO $ newMVar []
  liftIO $ timeout (1 * 10 ^ 6) $ forever $ do
    z <- readChan chan
    putStrLn $ "read " <> show z
    hFlush stdout
    modifyMVar_ zs $ \zs' -> pure $ z:zs'
  zs' <- liftIO $ takeMVar zs

  liftIO $ putStrLn $ "got " <> show zs'
  liftIO $ putStrLn $ "expected " <> show xs
  liftIO $ hFlush stdout

  assert $ Set.fromList zs' == Set.fromList xs
