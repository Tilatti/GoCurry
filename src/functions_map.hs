module FunctionMap (initFunMap, getFunSelectors, getFunction) where

import Data.Map (Map)
import qualified Data.Map as Map

type ReplyFunction = String -> String
type ReplyFunctionMap = Map String ReplyFunction
type MReplyFunction = Maybe ReplyFunction


echo :: ReplyFunction
echo input = input

aboutPrint :: ReplyFunction
aboutPrint input = "GoCurry a gopher server written by Alban Tilatti"

serverStat :: ReplyFunction
serverStat input = ""
--serverStat input = "The server work on host : " ++ config_hostname ++ ", " ++
--		   "with the port : " ++ config_port ++ "\n" ++

addFunction :: String -> ReplyFunction -> ReplyFunctionMap -> ReplyFunctionMap
addFunction key func map = Map.insert key func map

getFunction :: String -> ReplyFunctionMap -> MReplyFunction
getFunction key map = Map.lookup key map

createFunMap :: [(String, ReplyFunction)] -> ReplyFunctionMap
createFunMap = Map.fromList

extractFunMap :: ReplyFunctionMap -> [(String, ReplyFunction)]
extractFunMap = Map.toList

initFunMap :: ReplyFunctionMap
initFunMap = createFunMap [("echo", echo), ("about", aboutPrint)]

getFunSelectors :: ReplyFunctionMap -> [String]
getFunSelectors = Map.keys
