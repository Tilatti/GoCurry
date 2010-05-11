module ReplyRequest where

import System.Directory
import System.IO
import Monad (join)
import FunctionMap
import Maybe


reply_request :: FilePath -> Handle -> IO ()
reply_request "" channel =
  do
    get_cache_content "./" channel
reply_request request_line channel =
  do
    is_dir <- doesDirectoryExist request_line
    is_file <- doesFileExist request_line
    get_ressource request_line (is_dir, is_file) channel


get_ressource :: FilePath -> (Bool, Bool) -> Handle -> IO ()
get_ressource request_line (is_dir, is_file) channel
  | is_dir = get_cache_content request_line channel
  | is_file = get_file_content request_line channel
  | otherwise = apply_reply_function request_line channel
-- | otherwise = do print "Unknow descriptor"


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



apply_reply_function :: String -> Handle -> IO ()
apply_reply_function request_line channel =
 let
   reply_function = fromJust (getFunction request_line initMap)
 in
  do
    hPutStr channel (reply_function request_line)


get_func_map_content :: Handle -> IO ()
get_func_map_content channel = get_func_map_content_rec channel (extractFunMap initMap)
  where
   get_func_map_content_rec ::  Handle -> [(String, ReplyFunction)] -> IO ()
   get_func_map_content_rec channel [] = do hPutStrLn channel ""
   get_func_map_content_rec channel ((key, func):map) =
     do
       hPutStrLn channel 
       hPutStrLn channel key
