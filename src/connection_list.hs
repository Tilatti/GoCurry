module ConnectionList where

import Network
import Data.Maybe (Maybe, fromJust, isJust, isNothing)
import Control.Concurrent
import System.IO

type ConnectionId = Int
data Connection = Connection {
		    		id :: ConnectionId,
		    		channel :: Handle,
		    		hostname :: HostName,
		    		port :: PortNumber
		  	     }
type MConnection = Maybe Connection
type Connections = [Connection]

instance Show Connection where
  show connection = "(" ++ show (hostname connection) ++ ", " ++
		    	   show (port connection) ++ ")"

