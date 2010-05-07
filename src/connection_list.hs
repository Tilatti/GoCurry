module ConnectionList where

import Network
import Data.Maybe (Maybe, fromJust, isJust, isNothing)
import Control.Concurrent
import System.IO

type ConnectionId = Int
type Connection = (ThreadId, ConnectionId, Handle, HostName, PortNumber)
type MConnection = Maybe Connection
type Connections = [Connection]

get_connection_id::Connections -> ConnectionId -> MConnection
get_connection_id ((thread, id, handle, hostname, portnumber):conns) search_id =
    if (id == search_id) then
      Just (thread, id, handle, hostname, portnumber)
    else
      get_connection_id conns search_id
get_connection_id [] _ = Nothing

get_connection_thread::Connections -> ThreadId -> MConnection
get_connection_thread ((thread, id, handle, hostname, portnumber):conns) search_thread =
    if (thread == search_thread) then
      Just (thread, id, handle, hostname, portnumber)
    else
      get_connection_thread conns search_thread
get_connection_thread [] _ = Nothing

get_id :: Connection -> ConnectionId
get_id (_, id, _, _, _) = id

get_thread :: Connection -> ThreadId
get_thread (thread, _, _, _, _) = thread


