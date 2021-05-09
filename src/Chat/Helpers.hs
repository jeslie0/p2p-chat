module Chat.Helpers where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent
import qualified Data.ByteString.Char8 as C

getHi :: Socket -> IO ()
getHi sock = do
  msg <- recv sock 1024
  print $ C.unpack msg
  getHi sock

sendHi :: Socket -> IO ()
sendHi sock = do
  myMsg <- getLine
  if myMsg == "[^" then do
    close sock
    else do
    sendAll sock $ C.pack myMsg
    sendHi sock

communicate :: Socket -> IO ()
communicate sock = do
  (soc, _) <- accept sock
  print "Connected (I think). Type '[^' to close connection."
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

  print "Connected (I think). Type '[^' to close connection."
  forkIO $ getHi sock
  sendHi sock

  close sock


hostServer :: (Socket -> IO ()) -> IO ()
hostServer f = do
  addrInfos <- getAddrInfo
               (Just (defaultHints
                      {addrFlags = [AI_PASSIVE]}))
               Nothing  (Just "1995")
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 1

  f sock
  close sock
