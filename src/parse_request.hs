module ReplyRequest where

import System.Directory
import System.IO

import Monad (join)
import Maybe

import Network.Socket
import System.Posix.Syslog

import Config (config_port, config_hostname)
import ConnectionList
import FunctionMap

import FileUtils

reply_request :: FilePath -> Connection -> IO ()
reply_request "" connection =
  do
    get_cache_content "./" (channel connection)
reply_request request_line connection =
  do
    is_dir <- doesDirectoryExist request_line
    is_file <- doesFileExist request_line
    get_ressource connection request_line (is_dir, is_file) (channel connection)


get_ressource :: Connection -> FilePath -> (Bool, Bool) -> Handle -> IO ()
get_ressource connection request_line (is_dir, is_file) channel
  | is_dir = get_cache_content request_line channel
  | is_file = get_file_content request_line channel
  | otherwise = apply_reply_function connection request_line channel

get_dir_entries :: FilePath -> IO String
get_dir_entries pathname =
  do
    filepaths <- getDirectoryContents pathname
    return (get_dir_entries_rec filepaths)
    where
      get_dir_entries_rec :: [FilePath] -> String
      get_dir_entries_rec (filename : filenames) =
          build_entry_file (get_file_type filename) filename config_port config_hostname ++ get_dir_entries_rec filenames
      get_dir_entries_rec [] = ""


get_cache_content :: FilePath -> Handle -> IO ()
get_cache_content dir_path channel =
  do
    get_func_map_content channel
    get_file_content (dir_path ++ "/.cache") channel


get_file_content :: FilePath -> Handle  -> IO ()
get_file_content file_path channel =
  do
    s <- readFile file_path
    hPutStr channel s


apply_reply_function :: Connection -> String -> Handle -> IO ()
apply_reply_function connection request_line channel =
 let
   reply_function = (getFunction request_line initMap)
 in
   if (not (isJust reply_function)) then
     do
       syslog Warning ("Unknow descriptor '" ++ request_line++
       		       "' from : " ++ (show connection))
       hPutStrLn channel "Unknow descriptor"
   else
     do
       hPutStr channel ((fromJust reply_function) request_line)


get_func_map_content :: Handle -> IO ()
get_func_map_content channel = get_func_map_content_rec channel (getSelectors initMap)
  where
   get_func_map_content_rec :: Handle -> [String] -> IO ()
   get_func_map_content_rec channel [] = do hPutStrLn channel ""
   get_func_map_content_rec channel (selector:map) =
     do
       hPutStrLn channel (build_entry 0 selector selector config_port config_hostname)
       get_func_map_content_rec channel map

build_entry_file :: FileType -> FilePath -> PortNumber -> String -> String
build_entry_file file_type pathname port hostname =
  build_entry file_type pathname pathname port hostname

build_entry :: FileType -> String -> String -> PortNumber -> String -> String
build_entry file_type selector_name selector port hostname =
  (show file_type) ++ selector_name ++ "\t" ++ selector ++
  "\t" ++ hostname ++ "\t" ++ (show port) ++ "\n"
