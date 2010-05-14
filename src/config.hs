module Config where

import Network.Socket

config_port :: PortNumber
config_port = 70

config_hostname :: String
config_hostname = "localhost"

pid_file :: String
pid_file = "/var/run/gopherd.pid"
