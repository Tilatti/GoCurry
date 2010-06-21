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
import DirUtils

reply_request :: FilePath -> Connection -> IO ()
reply_request "" connection =
  do
    get_dir_content "./" (channel connection)
reply_request request_line connection =
  do
    is_dir <- doesDirectoryExist request_line
    is_file <- doesFileExist request_line
    get_ressource connection request_line (is_dir, is_file) (channel connection)


get_ressource :: Connection -> FilePath -> (Bool, Bool) -> Handle -> IO ()
get_ressource connection request_line (is_dir, is_file) channel
  | is_dir = get_dir_content request_line channel
  | is_file = get_file_content request_line channel
  | otherwise = apply_reply_function connection request_line channel


-- Send a list of descriptor
get_dir_content :: FilePath -> Handle -> IO ()
get_dir_content dir_path channel =
  let
    cache_file = dir_path ++ "/.cache"
  in
    do
      has_cache <- doesFileExist cache_file
      get_func_map_content channel
      if has_cache
	then get_file_content (dir_path ++ "/.cache") channel
	else do sending_str <- get_dir_entries dir_path
		hPutStr channel sending_str

-- Send a file
get_file_content :: FilePath -> Handle  -> IO ()
get_file_content file_path channel =
  do
    s <- readFile file_path
    hPutStr channel s


-- Send a result of a function invocation
apply_reply_function :: Connection -> String -> Handle -> IO ()
apply_reply_function connection request_line channel =
 let
   reply_function = (getFunction request_line initMap)
 in
   if (not (isJust reply_function)) then
     do
       syslog Warning ("Unknow descriptor '" ++ request_line ++
       		       "' from : " ++ (show connection))
       hPutStrLn channel "Unknow descriptor"
   else
     do
       hPutStr channel ((fromJust reply_function) request_line)

-- Send a list of descriptor
get_dir_entries :: FilePath -> IO String
get_dir_entries pathname =
  do
    file_infos <- list_dir pathname
    return (get_dir_entries_rec file_infos)
    where
      get_dir_entries_rec :: [(FilePath, Bool)] -> String
      get_dir_entries_rec file_infos = foldr cons_dir_entries "" file_infos

      cons_dir_entries :: (FilePath, Bool) -> String -> String
      cons_dir_entries (filename, is_dir)  entries
        | is_dir =
	    if (filename == "..") || (filename == ".") then
	      (++) "" entries
	    else
	      (++) (build_entry_dir filename) entries
	| otherwise =
	    (++) (build_entry_file' filename) entries


get_func_map_content :: Handle -> IO ()
get_func_map_content channel = get_func_map_content_rec channel (getSelectors initMap)
  where
   get_func_map_content_rec :: Handle -> [String] -> IO ()
   get_func_map_content_rec channel [] = do hPutStrLn channel ""
   get_func_map_content_rec channel (selector:map) =
     do
       hPutStrLn channel (build_entry 0 selector selector config_port config_hostname)
       get_func_map_content_rec channel map

build_entry_dir :: FilePath -> String
build_entry_dir pathname =
  build_entry_file 1 pathname config_port config_hostname

build_entry_file' :: FilePath -> String
build_entry_file' pathname =
  build_entry_file (get_file_type pathname) pathname config_port config_hostname

build_entry_file :: FileType -> FilePath -> PortNumber -> String -> String
build_entry_file file_type pathname port hostname =
  build_entry file_type pathname pathname port hostname

build_entry :: FileType -> String -> String -> PortNumber -> String -> String
build_entry file_type selector_name selector port hostname =
  (show file_type) ++ selector_name ++ "\t" ++ selector ++
  "\t" ++ hostname ++ "\t" ++ (show port) ++ "\r\n"
