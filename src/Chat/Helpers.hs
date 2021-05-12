module Chat.Helpers where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent
import System.IO

import qualified Data.ByteString.Char8 as C
import Data.List



getHi :: Socket -> IO ()
getHi sock = do
  msg <- recv sock 1024
  let unpackedMsg = C.unpack . cleanMsg $ msg
  if (unpackedMsg == "") || (unpackedMsg == "\n") then
    gracefulClose sock 5
    else do
    putStrLn unpackedMsg
    getHi sock

sendHi :: Socket -> IO ()
sendHi sock = do
  myMsg <- getLine

  if (myMsg == "\n") || (myMsg == "") then -- Won't send empty strings
    sendHi sock
    else
    if "!EXIT" `isSubsequenceOf` myMsg then do
    sendAll sock $ C.pack myMsg
    gracefulClose sock 5
    else do
    sendAll sock $ C.pack (myMsg ++ ['\n'])
    sendHi sock


makeLocalSocket :: PortNumber -> IO (Socket , SockAddr)
makeLocalSocket port = do
  addrInfos <- getAddrInfo
               (Just (defaultHints
                      {addrFlags = [AI_PASSIVE]}))
               Nothing  (Just $ show port)
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  return (sock , addrAddress serverAddr)


exitCommand :: C.ByteString -> Bool
exitCommand = C.isInfixOf (C.pack "!EXIT")

cleanMsg :: C.ByteString  -> C.ByteString  --removes \r\n from messages, if they exist
cleanMsg = C.filter (/= '\r') . C.filter (/= '\n')
