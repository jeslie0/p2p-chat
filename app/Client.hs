module Client where


import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent

import Chat.Helpers



main :: IO ()
main = connectToOther
