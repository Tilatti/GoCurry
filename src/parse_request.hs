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

import ExternExecutable (callExecutable, strToExecInfo)

import Control.Applicative

-- Send a list of descriptor
getDirContent :: FilePath -> Handle -> IO ()
getDirContent dir_path channel =
	let
		cache_file = dir_path ++ "/.cache"
	in
		do
			has_cache <- doesFileExist cache_file
			-- get_func_map_content channel -- Print the list of GoCurry internal function
			if has_cache
				then getFileContent cache_file channel
				else get_dir_entries dir_path channel


-- Send a file
getFileContent :: FilePath -> Handle -> IO ()
getFileContent file_path channel =
	do
		s <- readFile file_path
		hPutStr channel s


-- Send a result of a function invocation
apply_reply_function :: Connection -> String -> IO ()
apply_reply_function connection request_line =
 let
   reply_function = (getFunction request_line initFunMap)
   ch = channel connection
 in
   if (not (isJust reply_function)) then
     do
       syslog Warning ("Unknow descriptor '" ++ request_line ++
       		       "' from : " ++ (show connection))
       hPutStrLn ch "Unknow descriptor"
   else
     do
       hPutStr ch ((fromJust reply_function) request_line)


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
					if (filename == "..") || (filename == ".") || (filename == "./") then
						entries
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
	if (parentDirName == "./") then
		build_entry file_type pathname pathname port hostname
	else
		build_entry file_type pathname (parentDirName ++ "/" ++ pathname) port hostname


build_entry :: FileType -> String -> String -> PortNumber -> String -> String
build_entry file_type selector_name selector port hostname =
  (show file_type) ++ selector_name ++ "\t" ++ selector ++
  "\t" ++ hostname ++ "\t" ++ (show port) ++ "\r\n"


type GopherReply = Connection -> String -> IO Bool

actionIsDir :: GopherReply
actionIsDir conn request_line =
	do
		is_dir <- doesDirectoryExist request_line
		if (is_dir)
			then
				do
					getDirContent request_line (channel conn)
					return True
			else
				return False

actionIsFile :: GopherReply
actionIsFile conn request_line =
	do
		is_file <- doesFileExist request_line
		if (is_file)
			then
				do
					getFileContent request_line (channel conn)
					return True
			else
				return False

actionIsCallToExec :: GopherReply
actionIsCallToExec conn request_line =
	let
		exec_info = strToExecInfo request_line
	in
		do
			if (isJust exec_info)
				then
					do
						callExecutable (fromJust exec_info) (channel conn)
						return True
				else
					return False

actionIsCallToFun :: GopherReply
actionIsCallToFun conn request =
	do
		apply_reply_function conn request
		return True

gopher_replies :: [GopherReply]
gopher_replies =
	[actionIsDir, actionIsFile, actionIsCallToExec, actionIsCallToFun]


applyGopherReply :: Connection -> String -> [GopherReply] -> IO ()
applyGopherReply connection request [] = return ()
applyGopherReply connection request (action : xs) =
  do
    action_result <- action connection request
    if (action_result)
      then
      	return ()
      else
				applyGopherReply connection request xs

replyRequest :: Connection -> String -> IO ()
replyRequest connection line =
	let
		ch = (channel connection)
		request_line = init line
	in
		if (request_line == "")
			then
				getDirContent "./" (channel connection)
			else
				applyGopherReply connection request_line gopher_replies
