module ReadCache where

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

parse_ressource_type :: Parser RessourceType
parse_ressource_type cs = (char ? isRessourceType >-> digitToInt) cs

parse_ressource_name :: Parser RessourceName
parse_ressource_name = letters

parseSep :: Parser Char
parseSep = tab

parse_ressource_descriptor :: Parser RessourceDescriptor
parse_ressource_descriptor = letters

parse_ressource_server :: Parser RessourceServer
parse_ressource_server = letters

parse_port :: Parser Port
parse_port = number

parse_cache_line :: Parser RessourceInformation
parse_cache_line cs =
  let
    ressource_fs_access = ((parse_ressource_type #- tab) # parse_ressource_name) #- tab
    ressource_net_access = ((parse_ressource_descriptor #- tab) # parse_ressource_server) #- tab
    ressource_port = parse_port
    -- All combined parsers
    parsing_ressource = (ressource_fs_access # ressource_net_access # ressource_port) #- (lit '\n')
  in
    case parsing_ressource cs of
      Nothing -> Nothing
      Just (result, "") -> Just ((decons_paire result), "")
        where
	  decons_paire :: (((a, b), (c, d)), e) -> (a, b, c, d, e)
	  decons_paire (((n1, n2), (n3, n4)), n5) = (n1, n2, n3, n4, n5)
      _ -> Nothing

parse_cache_file :: Parser [RessourceInformation]
parse_cache_file = parse_iter parse_cache_line

readCacheFile :: String -> Maybe [RessourceInformation]
readCacheFile cache_content =
  let
    parse_result = (parse_cache_file cache_content)
  in
    if (isJust parse_result) then
      Just (fst (fromJust parse_result))
    else
      Nothing

getDescriptorList :: [RessourceInformation] -> [RessourceDescriptor]
getDescriptorList ressource_information_list = map getDescriptor ressource_information_list

--main = do print (fromJust ((parse_ressource_type # parse_ressource_name) "9TEERIJOI"))
