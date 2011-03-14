module ReadCache where
-- module Main where

import Parsing
import Data.Char (isDigit, digitToInt)
import Data.Maybe

--Must be change for gopher+ implementation
type RessourceType = Int
type RessourceName = String
type RessourceDescriptor = String
type RessourceServer = String
type Port = Int

type RessourceInformation = (RessourceType,
			     RessourceName,
			     RessourceDescriptor,
			     RessourceServer,
			     Port)

getType :: RessourceInformation -> RessourceType
getType (ty, _, _, _, _) = ty

getDescriptor :: RessourceInformation -> RessourceDescriptor
getDescriptor (_, _, desc, _, _) = desc

isRessourceType :: Char -> Bool
isRessourceType = isDigit


parseSep :: Parser Char
parseSep = lit '\t'


parse_ressource_type :: Parser RessourceType
parse_ressource_type = (char ? isRessourceType >-> digitToInt)

parse_ressource_name :: Parser RessourceName
parse_ressource_name = letters #- parseSep

parse_ressource_descriptor :: Parser RessourceDescriptor
parse_ressource_descriptor = letters #- parseSep

parse_ressource_server :: Parser RessourceServer
parse_ressource_server = letters #- parseSep

parse_port :: Parser Port
parse_port = number #- parseSep

parse_cache_line :: Parser RessourceInformation
parse_cache_line =
  glue5Parser parse_ressource_type
  	      parse_ressource_name
	      parse_ressource_descriptor
  	      parse_ressource_server parse_port

parse_cache_file :: Parser [RessourceInformation]
parse_cache_file = parse_iter parse_cache_line

readCacheFile :: String -> Maybe [RessourceInformation]
readCacheFile cache_content = applyParser parse_cache_file cache_content

getDescriptorList :: [RessourceInformation] -> [RessourceDescriptor]
getDescriptorList ressource_information_list = map getDescriptor ressource_information_list

-- main = do print (fromJust (applyParser double "cc"))
