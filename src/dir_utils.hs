module DirUtils where

import System.Directory
import System.IO
import Control.Monad

cons_list_dir_ele :: FilePath -> IO (FilePath, Bool)
cons_list_dir_ele filepath =
  do
    is_dir <- doesDirectoryExist filepath
    return (filepath, is_dir)


-- Return a list of Filepaths
list_dir :: FilePath -> IO [(FilePath, Bool)]
list_dir pathname =
  do
    filepaths <- getDirectoryContents pathname
    mapM cons_list_dir_ele filepaths
