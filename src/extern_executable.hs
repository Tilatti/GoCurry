module ExternExecutable where
--module Main where

import Parsing
import Data.Maybe
import System.Process (readProcess)
import System.IO (hPutStr, Handle, stdout)

data CallType = Simple | Python | Shell
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
callInterpreter (Python, file) = return ""
callInterpreter (Shell, file) = return ""

{-
main =
  do
		exec_info <- return $ (fromJust (strToExecInfo "#!call:/bin/ls"))
		result <- (callExecutable exec_info stdout)
		print (snd exec_info)
		print result -}
