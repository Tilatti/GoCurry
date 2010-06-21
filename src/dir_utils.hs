module DirUtils where

import System.Directory
import System.IO
import Control.Monad

cons_list_dir_ele :: FilePath -> FilePath -> IO (FilePath, Bool)
cons_list_dir_ele parentDirName filepath =
  do
    is_dir <- doesDirectoryExist (parentDirName ++ "/" ++ filepath)
    return (filepath, is_dir)


-- Return a list of Filepaths
list_dir :: FilePath -> IO [(FilePath, Bool)]
list_dir dirname =
  do
    filepaths <- getDirectoryContents dirname
    mapM (cons_list_dir_ele dirname) filepaths
