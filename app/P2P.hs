module P2P where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent

import Chat.Helpers

main :: IO ()
main = undefined
-- Set up remote location to connect to
  -- forkIO  connectToOther

  -- Set up socket to listen on
  -- hostServer 2000  communicate
