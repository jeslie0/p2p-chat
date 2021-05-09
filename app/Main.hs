module Main where

import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent
    ( forkIO, newEmptyMVar, tryTakeMVar, tryPutMVar, MVar )

import Data.List

 -- :: Socket -> IO ()

main :: IO ()
main = do
  addrinfos <- getAddrInfo
               (Just (defaultHints
                      {addrFlags = [AI_PASSIVE]}))
               Nothing  (Just "1995")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  foo sock
  close sock

foo :: Socket -> IO ()
foo sock = do
  (soc, _) <- accept sock
  forkIO $ getHi soc
  sendHi soc
  close soc

getHi :: Socket -> IO ()
getHi conn = do
  msg <- recv conn 1024
  print $ C.unpack msg
  getHi conn

sendHi :: Socket -> IO ()
sendHi conn = do
  myMsg <- getLine
  sendAll conn $ C.pack myMsg
  sendHi conn

communicate :: Socket -> IO ()
communicate conn = do
  (soc, _) <- accept conn
  forkIO $ getHi soc
  sendHi soc

foo1 :: Socket -> IO ()
foo1 sock = do
  msg <- getLine
  sendAll sock $ C.pack msg
  foo1 sock

sendMess :: IO ()
sendMess = do
  addrinfos <- getAddrInfo
               (Just (defaultHints
                      {addrFlags = [AI_PASSIVE]}))
               Nothing  (Just "1995")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  foo1 sock



-- Need to make a program that both listens to a port and can search for a given port and try to connect to it

p2p :: IO ()
p2p = do
  -- Set up remote location to connect to
  forkIO  connectToOther

  -- Set up socket to listen on
  hostServer  communicate

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

  forkIO $ getHi sock
  sendHi sock

  close sock

-- main :: IO ()
-- main = putStrLn "Booo"
