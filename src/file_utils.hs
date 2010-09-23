module FileUtils where

import System.Directory
import System.IO
import System.Cmd
import System (ExitCode)

type FileType = Int

get_file_type :: FilePath -> FileType
get_file_type pathname = 0

moveFile :: FilePath -> FilePath -> IO ExitCode
moveFile file newLocation = rawSystem "mv" ["-f", file, newLocation]

moveDirectory :: FilePath -> FilePath -> IO ExitCode
moveDirectory dir newLocation = rawSystem "mv" ["-f", dir, newLocation]

renameFile :: FilePath -> FilePath -> IO ExitCode
renameFile file newName = rawSystem "mv" ["-f", file, newName]

renameDirectory :: FilePath -> FilePath -> IO ExitCode
renameDirectory dir newName = rawSystem "mv" ["-f", dir, newName]

copyFile :: FilePath -> FilePath -> IO ExitCode
copyFile file destination = rawSystem "cp" ["-f", file, destination]

copyDirectory :: FilePath -> FilePath -> IO ExitCode
copyDirectory dir newLocation = rawSystem "cp" ["-rf", dir, newLocation]
