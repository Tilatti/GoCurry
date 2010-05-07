module ReadCache where

import Parsing
import Data.Char (isDigit, digitToInt)
import Data.Maybe

--Must be change for gopher+ implementation
type RessourceType = Int
type RessourceName = String
type RessourceLocation = String
type RessourceServer = String
type Port = Int

type RessourceInformation = (RessourceType,
			     RessourceName,
			     RessourceLocation,
			     RessourceServer,
			     Port)

isRessourceType :: Char -> Bool
isRessourceType = isDigit

parse_ressource_type::Parser RessourceType
parse_ressource_type cs = (char ? isRessourceType >-> digitToInt) cs

parse_ressource_name::Parser RessourceName
parse_ressource_name = letters

--main = do print (fromJust ((parse_ressource_type # parse_ressource_name) "9TEERIJOI"))
