module Client where


import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent

import Chat.Helpers
import System.Environment


connectToOther :: String -> String -> IO ()
connectToOther ip port = do
  addrInfo <- getAddrInfo (Just (defaultHints
                                 {addrFlags = [AI_PASSIVE]}))
              (Just ip) --fix
              (Just port) --fix
  let serverAddr = head addrInfo

  sock <- socket AF_INET Stream defaultProtocol
  connect sock (addrAddress serverAddr)

  forkIO $ getHi sock
  sendHi sock

main :: IO ()
main = do
  (ip:port:_) <- getArgs
  connectToOther ip port
