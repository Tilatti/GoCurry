module ParseGopher where

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
parse_ressource_type = char ? isRessourceType >-> digitToInt

parse_field :: Parser String
parse_field = (parse_iter (char ? isNotTab)) #- tab

parse_ressource_name :: Parser RessourceName
parse_ressource_name = parse_field

parse_ressource_location :: Parser RessourceLocation
parse_ressource_location = parse_field

parse_ressource_server :: Parser RessourceServer
parse_ressource_server = parse_field

parse_port :: Parser Port
parse_port = number

toRessourceInformation :: ((((Int, String), String), String), Int) -> RessourceInformation
toRessourceInformation ((((req_type, name), location), server), port) =
  (req_type, name, location, server, port)

parse_line :: Parser RessourceInformation
parse_line =
  (parse_ressource_type # parse_ressource_name  # parse_ressource_location #
  parse_ressource_server # parse_port) >-> toRessourceInformation

--main = do
-- print (parse_line "1CIA World Factbook\t/Archives/mirrors/textfiles.com/politics/CIA\tgopher.quux.org\t70")
