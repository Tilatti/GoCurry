module Opts (Flag, compileOpts, is_help, exec_actions) where

import System.Console.GetOpt
import Data.Maybe (isJust)
import List (find)

type OptsPredicat = [Flag] -> Bool

data Flag =
 	  Version
        | Verbose
	| Dynamic
	| Help
	deriving Eq

options :: [OptDescr Flag]
options =
  [
    Option ['h'] ["--help"] (NoArg Help) "Show Help",
    Option ['v'] ["--verbose"] (NoArg Verbose) "Verbose mode",
    Option ['V'] ["--version"] (NoArg Version) "Show version",
    Option ['d'] ["--dynamic"] (NoArg Dynamic) "Don't use .cache files"
  ]

compileOpts :: [String] -> IO ([Flag], [String])
compileOpts argv =
  case getOpt RequireOrder options argv of
   (opts, non_opts, []) -> return (opts, non_opts)
   (_, _, errs) -> ioError(userError (concat errs))


is_verbose :: [Flag] -> Bool
is_verbose = isJust . find (Verbose ==)

is_version :: [Flag] -> Bool
is_version = isJust . find (Version ==)

is_dynamic :: [Flag] -> Bool
is_dynamic = isJust . find (Dynamic ==)

is_help :: [Flag] -> Bool
is_help = isJust . find (Help ==)

is_in_flag :: Flag -> [Flag] -> Bool
is_in_flag flag = isJust . find (flag ==)


-- Print the usage
print_help :: IO ()
print_help =
  do
    print "Usage : gopherd [-v,-V,-d,-h]"

-- Print the current version
print_version :: IO ()
print_version =
  do
    print "GoCurry version 0.1"

actions :: [(Flag, IO ())]
actions =
  [
    (Version, print_version),
    (Help, print_help)
  ]

exec_actions :: [Flag] -> IO Bool
exec_actions [] = return False
exec_actions flags = exec_actions_rec actions flags
  where
    exec_actions_rec :: [(Flag, IO ())] -> [Flag] -> IO Bool
    exec_actions_rec ((action_flag, action) : actions) flags =
      if (is_in_flag action_flag flags)
	then
	  do
	    action
	    return True
	else
	  exec_actions_rec actions flags
    exec_actions_rec [] flags = return False
