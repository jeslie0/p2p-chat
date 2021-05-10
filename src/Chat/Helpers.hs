module Chat.Helpers where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent
import qualified Data.ByteString.Char8 as C

import Data.List

getHi :: Socket -> IO ()
getHi sock = do
  msg <- recv sock 1024
  let unpackedMsg = C.unpack msg
  if "!EXIT" `isSubsequenceOf` unpackedMsg then do
    print "Client disconnected"
    close sock
    else do
    print unpackedMsg
    getHi sock

sendHi :: Socket -> IO ()
sendHi sock = do
  myMsg <- getLine
  if "!EXIT" `isSubsequenceOf` myMsg then do
    sendAll sock $ C.pack "!Exit"
    close sock
    else do
    sendAll sock $ C.pack myMsg
    sendHi sock

communicate :: Socket -> IO ()
communicate sock = do
  (soc, _) <- accept sock
  print "Connected (I think). Type '!EXIT' to close connection."
  forkIO $ getHi soc
  sendHi soc



connectToOther :: IO ()
connectToOther = do
  putStrLn "Enter desired IP or url."
  ip <- getLine
  putStrLn "Enter desired port."
  port <- getLine

  addrInfo <- getAddrInfo (Just (defaultHints
                                 {addrFlags = [AI_PASSIVE]}))
              (Just ip) --fix
              (Just port) --fix
  let serverAddr = head addrInfo

  sock <- socket AF_INET Stream defaultProtocol
  connect sock (addrAddress serverAddr)

  print "Connected (I think). Type '!EXIT' to close connection."
  forkIO $ getHi sock
  sendHi sock

  close sock


hostServer :: (Socket -> IO ()) -> IO ()
hostServer f = do
  (sock, sockAddr) <- makeLocalSocket 2000
  bind sock sockAddr
  listen sock 1

  f sock
  close sock


makeLocalSocket :: PortNumber -> IO (Socket , SockAddr)
makeLocalSocket port = do
  addrInfos <- getAddrInfo
               (Just (defaultHints
                      {addrFlags = [AI_PASSIVE]}))
               Nothing  (Just $ show port)
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  return (sock , addrAddress serverAddr)
