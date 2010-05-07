module ReplyRequest where

import System.Directory
import System.IO
import Monad (join)


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
  | otherwise = do print "Unknow descriptor"


get_cache_content :: FilePath -> Handle -> IO ()
get_cache_content dir_path channel =
  get_file_content (dir_path ++ "/.cache") channel


get_file_content :: FilePath -> Handle  -> IO ()
get_file_content file_path channel =
  do
    s <- readFile file_path
    hPutStr channel s
