module Server where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent

import Data.List
import qualified Data.ByteString.Char8 as C

import Chat.Helpers (makeLocalSocket, cleanMsg, exitCommand)
import System.Environment (getArgs)


type UserName = C.ByteString

hostServer :: PortNumber -> (Socket -> IO ()) -> IO ()
hostServer port f = do
  (sock, sockAddr) <- makeLocalSocket port
  bind sock sockAddr
  listen sock 1

  f sock

  close sock


foo :: Socket -> IO ()
foo sock = do
  mvarSocs <- newEmptyMVar :: IO (MVar [Socket])
  putMVar mvarSocs []

  listenAndSendServer mvarSocs sock



listenAndSendServer :: MVar [Socket] -> Socket -> IO () -- Listens for new connections and spawns a new process when they connect. Sends some nice messages to the server.
listenAndSendServer socsMvar sock = do
  (soc, _) <- accept sock
  modifyMVar_ socsMvar $ \socs -> return (soc:socs)

  sendAll soc $ C.pack "Enter username: "
  username <- recv soc 1024
  let cleanUserName = cleanMsg username
  sendAll soc $ C.pack "Hello " `mappend` cleanUserName `mappend` C.pack ("! Type `!EXIT` to leave" ++ ['\n'])

  socs <- readMVar socsMvar
  mapM_ (`sendAll` (cleanUserName `mappend` C.pack (" has joined the chat." ++ ['\n']))) (filter (/= soc) socs)

  forkIO $ serverThreadApp socsMvar cleanUserName soc
  listenAndSendServer socsMvar sock


serverThreadApp :: MVar [Socket] -> UserName -> Socket -> IO ()
serverThreadApp socsMvar username sock = do
  msg <- recv sock 1024
  socs <- readMVar socsMvar

  if exitCommand msg then do
    gracefulClose sock 5
    modifyMVar_ socsMvar $ \soc' -> return $ delete sock soc'
    mapM_ (`sendAll` (username `mappend` C.pack (" has left the chat." ++ ['\n']))) socs

    else do
    let niceMsg = username `mappend` C.pack ": " `mappend` msg

    mapM_ (`sendAll` niceMsg) (filter (/= sock) socs)
    putStrLn . C.unpack . cleanMsg $ niceMsg

    serverThreadApp socsMvar username sock



main :: IO ()
main = do
  (portStr:_) <- getArgs
  hostServer (read portStr :: PortNumber) foo
