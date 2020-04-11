{-# LANGUAGE LambdaCase #-}

module Async where

import Control.Applicative ((<|>))
import Control.Concurrent (forkFinally, ThreadId, throwTo)
import Control.Concurrent.STM (atomically, STM, throwSTM, orElse, retry)
import Control.Concurrent.STM.TMVar (TMVar(..), readTMVar, putTMVar, newEmptyTMVarIO)
import Control.Exception (SomeException, AsyncException(ThreadKilled), bracket)
import Control.Monad ((>=>), join)

type AsyncResult a = Either SomeException a
data Async a = Async ThreadId (STM (AsyncResult a))

instance Functor Async where
    fmap f (Async t stm) = Async t (fmap f <$> stm)

-- Can't implement Applicative / Monad instance because pure requires
-- threadId. `myThreadId :: IO ThreadId`, and we can't do arbitrary IO in
-- `pure`.

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyTMVarIO
    t <- forkFinally action (atomically . putTMVar var)
    pure $ Async t (readTMVar var)

cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

waitCatchSTM :: Async a -> STM (AsyncResult a)
waitCatchSTM (Async _ stm) = stm

waitCatch :: Async a -> IO (AsyncResult a)
waitCatch = atomically . waitCatchSTM

waitSTM :: Async a -> STM a
waitSTM = waitCatchSTM  >=> either throwSTM pure

wait :: Async a -> IO a
wait = atomically . waitSTM

-- waitAny and waitEither don't use withAsync, which gives exception safety
-- But they also don't return Async that can be combined that way.
-- So they won't cancel all actions if one receives an exception

waitAny :: Traversable t => t (Async a) -> IO a
waitAny as = atomically $ foldr (<|>) retry $ waitSTM <$> as

-- would like Traverable f => f (Async a) -> Async (f a)
-- but that requires Async to be applicative
waitAll :: Traversable f => f (Async a) -> IO (f a)
waitAll as = atomically $ traverse waitSTM as

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
    (Left <$> waitSTM a)
    <|>
    (Right <$> waitSTM b)

waitBoth :: Async a -> Async b -> IO (a, b)
waitBoth a1 a2 = atomically $ do
  r1 <- waitSTM a1 <|> (waitSTM a2 >> retry)
  r2 <- waitSTM a2
  pure (r1, r2)

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io op = bracket (async io) cancel op

concurrently :: IO a -> IO b -> IO (a, b)
concurrently = twoAsyncs waitBoth

race :: IO a -> IO b -> IO (Either a b)
race = twoAsyncs waitEither

twoAsyncs :: (Async a -> Async b -> IO c) -> IO a -> IO b  -> IO c
twoAsyncs c a b =
    withAsync a $ \asyncA ->
      withAsync b $ \asyncB ->
        c asyncA asyncB
