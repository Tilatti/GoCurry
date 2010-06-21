module FileUtils where

import System.Directory
import System.IO

type FileType = Int

get_file_type :: FilePath -> FileType
get_file_type pathname = 0
