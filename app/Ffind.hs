import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TChan, newTChanIO, readTChan, writeTChan)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import System.Directory (listDirectory, doesDirectoryExist)
import System.Environment (getArgs)

data Result = Found FilePath | Done
            deriving (Show)

main :: IO ()
main = do
    [name, dir] <- getArgs
    workQueue <- newTChanIO
    atomically $ writeTChan workQueue dir
    resultQueue <- newTChanIO
    traverse_ forkIO $ replicate numThreads (findFile name workQueue resultQueue)
    forever $ atomically (readTChan resultQueue) >>= putStrLn . show
  where numThreads = 8

findFile :: String -> TChan FilePath -> TChan Result -> IO ()
findFile name work result = do
    dir <- atomically $ readTChan work
    (dirs, files) <- listDirectory dir >>= partitionM doesDirectoryExist
    void $ atomically $ for dirs (writeTChan work . (dir <>))
    let matches = filter (== name) files
    void $ atomically $ for matches (writeTChan result . Found)
    findFile name work result
    -- how do I know to terminate

partitionM :: (a -> m b) -> [a] -> m ([a], [a])
partitionM = undefined
