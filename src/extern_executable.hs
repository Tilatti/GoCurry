module ExternExecutable where
-- module Main where

import Parsing
import Data.Maybe
import System.Process (readProcess)
import System.IO (hPutStr, Handle)

data CallType = Simple | Guile | Python | Shell
type ExecInfo = (CallType, String)

parseCallRequest :: Parser ExecInfo
parseCallRequest = ((accept "#!call:") -# (parse_return Simple)) # parseFilePath

strToExecInfo :: String -> Maybe ExecInfo
strToExecInfo = applyParser parseCallRequest


-- Call the executable and write his stdout to channel
callExecutable :: ExecInfo -> Handle -> IO ()
callExecutable exec_info channel =
  do
    out <- callInterpreter exec_info
    hPutStr channel out

callInterpreter :: ExecInfo -> IO String
callInterpreter (Simple, file) =
  do
    readProcess file [] ""
callInterpreter (Guile, file) = return ""
callInterpreter (Python, file) = return ""
callInterpreter (Shell, file) = return ""

{-
main =
  do
    result <- (callExecutable "#!call:/bin/ls")
    print result -}
