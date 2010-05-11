module GopherConnection where

import Control.Concurrent
import System.Exit
import System.Posix.IO
import System.Posix.Process

import Network
import System.IO
import Data.Maybe (Maybe, fromJust, isJust, isNothing)

import ReplyRequest (reply_request)
import ConnectionList (Connections, ConnectionId, MConnection, ConnectionId, Connection)

import Config (port, hostname)

--Must be remplaced by a better implementation
chomp :: String -> String
chomp (c:[]) = []
chomp (c:cs) = c : (chomp cs)

--The management loop for each clients
th_clientLoop :: Handle -> IO ()
th_clientLoop channel = do
			  input <- hGetLine channel
			  reply_request (chomp input) channel
			  hClose channel

--Accept connections on listen socket, and add them to the list
--It is a blocking instruction
accept_connection_thread :: Socket -> ConnectionId -> IO Connection
accept_connection_thread listen_socket last_id =
    do
      (new_handle, new_hostname, new_portnumber) <- accept listen_socket
      thread <- forkIO (th_clientLoop new_handle)
      return (thread, last_id + 1, new_handle, new_hostname, new_portnumber)

accept_loop :: Socket -> IO ()
accept_loop listen_socket = accept_loop_rec [] listen_socket
  where
    accept_loop_rec :: Connections -> Socket -> IO ()
    accept_loop_rec connections listen_socket =
	do
	  new_connection <- accept_connection_thread listen_socket 0
	  accept_loop_rec (new_connection : connections) listen_socket

listen_network =
  do
      listen_socket <- listenOn (PortNumber 70)
      accept_loop listen_socket
