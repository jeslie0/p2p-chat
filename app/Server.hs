module Server where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

import Control.Concurrent

import Chat.Helpers



main :: IO ()
main = hostServer communicate
