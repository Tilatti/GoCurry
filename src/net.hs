--TODO
-- + Syslog writing
-- + Demonisation
-- Chroot

module GopherConnection where

import Control.Concurrent
import System.Exit
import System.Posix.IO
import System.Posix.Process

import Network
import System.IO
import Data.Maybe (Maybe, fromJust, isJust, isNothing)

import ReplyRequest (reply_request)
import ConnectionList

import Config (config_port, config_hostname)

chomp :: String -> String
chomp = init


--The management loop for each clients
th_clientLoop :: Connection -> IO ()
th_clientLoop connection =
  let
    handle :: Handle
    handle = channel connection
  in
    do
      input <- hGetLine handle
      reply_request (chomp input) connection
      hClose handle


--Accept connections on listen socket, and add them to the list
--It is a blocking instruction
accept_connection_thread :: Socket -> ConnectionId -> IO Connection
accept_connection_thread listen_socket last_id =
    do
      (new_handle, new_hostname, new_portnumber) <- accept listen_socket
      new_connection <- return $ Connection (last_id + 1) new_handle new_hostname new_portnumber
      thread <- forkIO (th_clientLoop new_connection)
      return new_connection


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
      listen_socket <- listenOn (PortNumber config_port)
      accept_loop listen_socket
