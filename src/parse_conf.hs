module ParseConfig where

import Parsing
import System.IO
import Data.Maybe (fromJust)

type ConfigEntry = (String, String)
type ConfigEntries = [ConfigEntry]

parse_config_entry :: Parser ConfigEntry
parse_config_entry = parse_semicolon # word # (token parse_equal) #
		     alphaWord # parse_lineSep

parse_config_entries :: Parser ConfigEntries
parse_config_entries = parse_iter parse_config_entry

read_config :: FilePath -> (ConfigEntries, Bool)
read_config file_path =
  do
    file_handle <- openFile file_path ReadMode
    content <- getContents file_handle
    fromJust (parse_config_entries content)
