module FunctionMap where

import Data.Map (Map)
import qualified Data.Map as Map

type ReplyFunction = String -> String
type ReplyFunctionMap = Map String ReplyFunction
type MReplyFunction = Maybe ReplyFunction


echo :: ReplyFunction
echo input = input

aboutPrint :: ReplyFunction
aboutPrint input = "GoCurry a gopher server written by Alban Tilatti"


addFunction :: String -> ReplyFunction -> ReplyFunctionMap -> ReplyFunctionMap
addFunction key func map = Map.insert key func map

getFunction :: String -> ReplyFunctionMap -> MReplyFunction
getFunction key map = Map.lookup key map


createFunMap :: [(String, ReplyFunction)] -> ReplyFunctionMap
createFunMap = Map.fromList

extractFunMap :: ReplyFunctionMap -> [(String, ReplyFunction)]
extractFunMap = Map.toList


initMap :: ReplyFunctionMap
initMap = createFunMap [("echo", echo), ("about", aboutPrint)]

getSelectors :: ReplyFunctionMap -> [String]
getSelectors = Map.keys
