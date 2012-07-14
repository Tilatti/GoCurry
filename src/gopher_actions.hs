module GopherActions where

-- The function type  GopherReply
--   Connection : Connection descriptor
--   String : Remote request
--   return true => Correct action
--   return false => Incrorrect action
type GopherReply = Connection -> String -> IO Bool

actionIsDir :: GopherReply
actionIsDir conn request_line =
  do
    is_dir <- doesDirectoryExist request_line
    if (is_dir)
      then
        get_dir_content request_line (channel conn) >>
				return True
      else
				return False

actionIsFile :: GopherReply
actionIsFile conn request_line =
  do
    is_file <- doesFileExist request_line
    if (is_file)
      then
				get_file_content request_line (channel conn) >>
				return True
      else
				return False

actionIsCallToExec :: GopherReply
actionIsCallToExec conn request_line =
  let
    exec_info = parseCallRequest request_line
  in
    do
      if (isJust exec_info)
  			then
					callExecutable (fst (fromJust exec_info)) (channel conn) >>
					return True
				else
					return False

actionIsCallToFun :: GopherReply
actionIsCallToFun conn request =
  do
    apply_reply_function conn request
    return True

gopher_replies :: [GopherReply]
gopher_replies = [actionIsDir,
		  actionIsFile,
		  actionIsCallToExec,
		  actionIsCallToFun]


applyGopherReply :: Connection -> String -> [GopherReply] -> IO ()
applyGopherReply connection request [] = return ()
applyGopherReply connection request (action : xs) =
  do
    action_result <- action connection request
    if (action_result)
      then
      	return ()
      else
				applyGopherReply connection request xs
