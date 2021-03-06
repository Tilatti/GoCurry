-- gocurry - a gopher internet protocol server
-- Copyright (C) 2010 Tilatti Alban <tilatt_a@epita.fr>

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main where

import GopherConnection (listen_network)
import Config (pid_file)

import System.IO
import System.Posix.Types
import System.Posix.Process

import System.Environment
import Opts (exec_actions, compileOpts)

write_pid :: ProcessID -> IO ()
write_pid pid =
	do
		file_handle <- openFile pid_file WriteMode
		hPutStr file_handle (show pid)
		hClose file_handle


main =
	do
		argv <- getArgs
		opts <- compileOpts argv
		have_arg <- exec_actions (fst opts)
		if not have_arg
			then
				do
					pid <- forkProcess listen_network
					write_pid pid
			else
				do return ()
