module ExternExecutable where

import Parsing
import Data.Maybe
import System.Process

data CallType = Simple | Guile | Python
type ExecInfo = (CallType, String)

parseCallType :: Parser CallType
parseCallType = (accept "#!call:") -# (parse_return Simple)

parseCallRequest :: Parser ExecInfo
parseCallRequest = parseCallType # word

callExecutable :: String -> IO String
callExecutable request =
  do
    exec_info <- fromJust (parseCallRequest request)
    if ((fst exec_info) == Simple)
      then createProcess (proc (snd exec_info) [])
      else ""

-- main = print (snd (fromJust (parseCallRequest "#!call:/bin/ls")))
