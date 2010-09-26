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

import ExternExecutable (callExecutable, parseCallRequest)


-- Send a list of descriptor
get_dir_content :: FilePath -> Handle -> IO ()
get_dir_content dir_path channel =
  let
    cache_file = dir_path ++ "/.cache"
  in
    do
      has_cache <- doesFileExist cache_file
      -- get_func_map_content channel -- Print the list of GoCurry internal function
      if has_cache
	then get_file_content (dir_path ++ "/.cache") channel
	else get_dir_entries dir_path channel


-- Send a file
get_file_content :: FilePath -> Handle -> IO ()
get_file_content file_path channel =
  do
    s <- readFile file_path
    hPutStr channel s


-- Send a result of a function invocation
apply_reply_function :: Connection -> String -> Handle -> IO ()
apply_reply_function connection request_line channel =
 let
   reply_function = (getFunction request_line initFunMap)
 in
   if (not (isJust reply_function)) then
     do
       syslog Warning ("Unknow descriptor '" ++ request_line ++
       		       "' from : " ++ (show connection))
       hPutStrLn channel "Unknow descriptor"
   else
     do
       hPutStr channel ((fromJust reply_function) request_line)


-- Send a list of descriptor in handle
get_dir_entries :: FilePath -> Handle -> IO ()
get_dir_entries pathname channel =
  do
    file_infos <- list_dir pathname
    hPutStr channel (foldr (cons_dir_entries pathname) "" file_infos)
    where
      cons_dir_entries :: String -> (FilePath, Bool) -> String -> String
      cons_dir_entries parentDirName (filename, is_dir)  entries
        | is_dir =
	    if (filename == "..") || (filename == ".") then
	      (++) "" entries
	    else
	      (++) (build_entry_dir parentDirName filename) entries
	| otherwise =
	    (++) (build_entry_file' parentDirName filename) entries


get_func_map_content :: Handle -> IO ()
get_func_map_content channel = get_func_map_content_rec channel (getFunSelectors initFunMap)
  where
   get_func_map_content_rec :: Handle -> [String] -> IO ()
   get_func_map_content_rec channel [] = do hPutStrLn channel ""
   get_func_map_content_rec channel (selector:map) =
     do
       hPutStrLn channel (build_entry 0 selector selector config_port config_hostname)
       get_func_map_content_rec channel map


build_entry_dir :: FilePath -> FilePath -> String
build_entry_dir parentDirName pathname =
  build_entry_file 1 parentDirName pathname config_port config_hostname


build_entry_file' :: FilePath -> FilePath -> String
build_entry_file' parentDirName pathname =
  let
    file_type = get_file_type pathname
  in
    build_entry_file file_type parentDirName pathname config_port config_hostname


build_entry_file :: FileType -> FilePath -> FilePath -> PortNumber -> String -> String
build_entry_file file_type parentDirName pathname port hostname =
  build_entry file_type pathname (parentDirName ++ "/" ++ pathname) port hostname


build_entry :: FileType -> String -> String -> PortNumber -> String -> String
build_entry file_type selector_name selector port hostname =
  (show file_type) ++ selector_name ++ "\t" ++ selector ++
  "\t" ++ hostname ++ "\t" ++ (show port) ++ "\r\n"


replyRequest :: FilePath -> Connection -> IO ()
replyRequest "" connection =
  do
    get_dir_content "./" (channel connection)
replyRequest request_line connection =
  let
    ch = (channel connection)
  in
    do
      is_dir <- doesDirectoryExist request_line
      if (is_dir)
	then get_dir_content request_line ch
	else
	  do
	    is_file <- doesFileExist request_line
	    if (is_file)
	      then get_file_content request_line ch
	      else let
		     exec_info = parseCallRequest request_line
		   in
		     do
		       if (isJust exec_info)
			 then callExecutable (fst (fromJust exec_info)) ch
			 else apply_reply_function connection request_line ch
