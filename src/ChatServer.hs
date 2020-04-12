{-# LANGUAGE RecordWildCards #-}

module ChatServer where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (
    STM, newTChan, newTVar, TVar, TChan, writeTChan, newTVarIO, readTVar
  , atomically, writeTVar, readTChan, modifyTVar
  )
import Control.Exception (finally, mask)
import Control.Monad (forever, void, join, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.Socket (
    withSocketsDo, accept, socket, Family(AF_INET), SocketType(Stream)
  , defaultProtocol, bind, SockAddr(SockAddrInet), tupleToHostAddress, Socket
  , socketToHandle
  )
import System.IO (
    Handle, hClose, IOMode(ReadWriteMode), hSetNewlineMode, universalNewlineMode
  , hSetBuffering, BufferMode(LineBuffering), hPutStrLn, hGetLine
  )
import Text.Printf (printf, hPrintf)

----------------------------------------
-- Client

type ClientName = String

data Client = Client
  { name :: ClientName
  , handle :: Handle
  , kicked :: TVar (Maybe ClientName)
  , sendChan :: TChan Message
  }

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String

data Server = Server { clients :: TVar (Map ClientName Client) }

newClient :: ClientName -> Handle -> STM Client
newClient name handle =
    Client name handle <$> newTVar Nothing <*> newTChan

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
    writeTChan sendChan msg

newServer :: IO Server
newServer = Server <$> newTVarIO Map.empty

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = readTVar clients >>= mapM_ (flip sendMessage msg) . Map.elems

serverMain :: IO ()
serverMain = withSocketsDo $ do
  server <- newServer
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 44444 $ tupleToHostAddress (127, 0, 0, 1))
  printf "Listening on port %d\n" port
  forever $ do
    (s', addr) <- accept sock
    handle <- socketToHandle s' ReadWriteMode
    printf "Accepted connection from %s\n" (show addr)
    forkFinally (talk handle server) (const $ hClose handle)

port :: Int
port = 44444

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    readName
  where readName = do
          hPutStrLn handle "What is your name?"
          name <- hGetLine handle
          if null name
            then readName
            else mask $ \restore ->  do
              ok <- checkAddClient server name handle
              case ok of
                Nothing -> do
                  hPrintf handle "The name %s is in use, please choose another\n" name
                  readName
                Just c -> restore (runClient server c) `finally` removeClient server name

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Map.member name clientmap
    then pure Nothing
    else do client <- newClient name handle
            writeTVar clients $ Map.insert name client clientmap
            broadcast server $ Notice (name ++ " has connected")
            pure (Just client)

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} = do
    void $ race serve receive
  where receive = forever $ hGetLine handle >>= atomically . sendMessage client . Command
        serve = join $ atomically $ do
          k <- readTVar kicked
          case k of
            Just kicker -> pure $ hPutStrLn handle $ "You have been kicked by: " ++ kicker
            Nothing -> do
              msg <- readTChan sendChan
              return $ do
                continue <- handleMessage server client msg
                when continue $ serve

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
    case message of
      Notice msg -> output $ "*** " ++ msg
      Tell name msg -> output $ "*" ++ name ++ "*: " ++ msg
      Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
      Command msg ->
        case words msg of
          ["/kick", who] -> atomically (kick server who name) *> pure True
          "/tell" : who : what -> tell server client who (unwords what) *> pure True
          ["/quit"] -> pure False
          ('/':_) : _ -> hPutStrLn handle ("Unrecognized command: " ++ msg) *> pure True
          _ -> atomically (broadcast server $ Broadcast name msg) *> pure True
  where output s = hPutStrLn handle s *> pure True


removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} c = atomically $
  modifyTVar clients (Map.delete c)

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} kickedClient kickingClient = do
    cs <- readTVar clients
    case Map.lookup kickedClient cs of
      Just c -> modifyTVar (kicked c) $ maybe (Just kickingClient) Just
      Nothing -> pure ()

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} client@Client{..} to msg = atomically $ do
    cs <- readTVar clients
    case Map.lookup to cs of
      Just c -> writeTChan sendChan $ Tell name msg
      Nothing -> pure ()
