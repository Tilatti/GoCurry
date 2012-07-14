--TODO
-- + Syslog writing
-- + Demonisation
-- Chroot

module GopherConnection where

-- Haskell modules

import Control.Concurrent
import System.Exit
import System.Posix.IO
import System.Posix.Process

import Network
import System.IO
import Data.Maybe (Maybe, fromJust, isJust, isNothing)

-- Local modules

import ReplyRequest (replyRequest)
import ConnectionList

import Config (config_port, config_hostname)

-- #############
-- Client Loop :
-- #############

--The management loop for each clients
th_clientLoop :: Connection -> IO ()
th_clientLoop connection =
  let
    handle :: Handle
    handle = channel connection
  in
    do
      input <- hGetLine handle
      replyRequest connection input
      hClose handle

-- #############
-- Accept loop :
-- #############

--Accept connections on listen socket, and add them to the list
--It is a blocking instruction
accept_connection_thread :: Socket -> ConnectionId -> IO Connection
accept_connection_thread listen_socket last_id =
	do
		(new_handle, new_hostname, new_portnumber) <- accept listen_socket
		new_connection <- return $ Connection last_id  new_handle new_hostname new_portnumber
		thread <- forkIO (th_clientLoop new_connection)
		return new_connection


accept_loop :: Socket -> IO ()
accept_loop listen_socket = accept_loop_rec [] 0 listen_socket
	where
		accept_loop_rec :: Connections -> ConnectionId -> Socket -> IO ()
		accept_loop_rec connections id listen_socket =
			do
				new_connection <- accept_connection_thread listen_socket id
				accept_loop_rec (new_connection : connections) (id + 1) listen_socket


listen_network =
	do
		listen_socket <- listenOn (PortNumber config_port)
		accept_loop listen_socket
