{-# INCLUDE <syslog.h> #-}
{-# LINE 1 "./Syslog.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "./Syslog.hsc" #-}
{- |
   Module      :  System.Posix.Syslog
   Copyright   :  (c) 2008 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Posix

   FFI bindings to Unix's @syslog(3)@. Process this file
   with @hsc2hs@ to obtain a Haskell module.
-}

module System.Posix.Syslog where

import System.IO
import Control.Exception ( bracket_ )
import Foreign.C


{-# LINE 22 "./Syslog.hsc" #-}

{-# LINE 25 "./Syslog.hsc" #-}


{-# LINE 29 "./Syslog.hsc" #-}


{-# LINE 33 "./Syslog.hsc" #-}

-- * Marshaled Data Types

-- |Log messages are prioritized.

data Priority
  = Emergency   -- ^ system is unusable
  | Alert       -- ^ action must be taken immediately
  | Critical    -- ^ critical conditions
  | Error       -- ^ error conditions
  | Warning     -- ^ warning conditions
  | Notice      -- ^ normal but significant condition
  | Info        -- ^ informational
  | Debug       -- ^ debug-level messages
  deriving (Eq, Bounded, Show)

instance Enum Priority where
  toEnum 0   = Emergency
{-# LINE 51 "./Syslog.hsc" #-}
  toEnum 1   = Alert
{-# LINE 52 "./Syslog.hsc" #-}
  toEnum 2    = Critical
{-# LINE 53 "./Syslog.hsc" #-}
  toEnum 3     = Error
{-# LINE 54 "./Syslog.hsc" #-}
  toEnum 4 = Warning
{-# LINE 55 "./Syslog.hsc" #-}
  toEnum 5  = Notice
{-# LINE 56 "./Syslog.hsc" #-}
  toEnum 6    = Info
{-# LINE 57 "./Syslog.hsc" #-}
  toEnum 7   = Debug
{-# LINE 58 "./Syslog.hsc" #-}
  toEnum i = error (showString "Syslog.Priority cannot be mapped from value " (show i))

  fromEnum Emergency = 0
{-# LINE 61 "./Syslog.hsc" #-}
  fromEnum Alert     = 1
{-# LINE 62 "./Syslog.hsc" #-}
  fromEnum Critical  = 2
{-# LINE 63 "./Syslog.hsc" #-}
  fromEnum Error     = 3
{-# LINE 64 "./Syslog.hsc" #-}
  fromEnum Warning   = 4
{-# LINE 65 "./Syslog.hsc" #-}
  fromEnum Notice    = 5
{-# LINE 66 "./Syslog.hsc" #-}
  fromEnum Info      = 6
{-# LINE 67 "./Syslog.hsc" #-}
  fromEnum Debug     = 7
{-# LINE 68 "./Syslog.hsc" #-}

-- |Syslog distinguishes various system facilities. Most
-- applications should log in 'USER'.

data Facility
  = KERN        -- ^ kernel messages
  | USER        -- ^ user-level messages (default unless set otherwise)
  | MAIL        -- ^ mail system
  | DAEMON      -- ^ system daemons
  | AUTH        -- ^ security\/authorization messages
  | SYSLOG      -- ^ messages generated internally by syslogd
  | LPR         -- ^ line printer subsystem
  | NEWS        -- ^ network news subsystem
  | UUCP        -- ^ UUCP subsystem
  | CRON        -- ^ clock daemon
  | AUTHPRIV    -- ^ security\/authorization messages (effectively equals 'AUTH' on some systems)
  | FTP         -- ^ ftp daemon (effectively equals 'DAEMON' on some systems)
  | LOCAL0      -- ^ reserved for local use
  | LOCAL1      -- ^ reserved for local use
  | LOCAL2      -- ^ reserved for local use
  | LOCAL3      -- ^ reserved for local use
  | LOCAL4      -- ^ reserved for local use
  | LOCAL5      -- ^ reserved for local use
  | LOCAL6      -- ^ reserved for local use
  | LOCAL7      -- ^ reserved for local use
  deriving (Eq, Bounded, Show)

instance Enum Facility where
  toEnum 0      = KERN
{-# LINE 97 "./Syslog.hsc" #-}
  toEnum 8      = USER
{-# LINE 98 "./Syslog.hsc" #-}
  toEnum 16      = MAIL
{-# LINE 99 "./Syslog.hsc" #-}
  toEnum 24    = DAEMON
{-# LINE 100 "./Syslog.hsc" #-}
  toEnum 32      = AUTH
{-# LINE 101 "./Syslog.hsc" #-}
  toEnum 40    = SYSLOG
{-# LINE 102 "./Syslog.hsc" #-}
  toEnum 48       = LPR
{-# LINE 103 "./Syslog.hsc" #-}
  toEnum 56      = NEWS
{-# LINE 104 "./Syslog.hsc" #-}
  toEnum 64      = UUCP
{-# LINE 105 "./Syslog.hsc" #-}
  toEnum 72      = CRON
{-# LINE 106 "./Syslog.hsc" #-}
  toEnum 80  = AUTHPRIV
{-# LINE 107 "./Syslog.hsc" #-}
  toEnum 88       = FTP
{-# LINE 108 "./Syslog.hsc" #-}
  toEnum 128    = LOCAL0
{-# LINE 109 "./Syslog.hsc" #-}
  toEnum 136    = LOCAL1
{-# LINE 110 "./Syslog.hsc" #-}
  toEnum 144    = LOCAL2
{-# LINE 111 "./Syslog.hsc" #-}
  toEnum 152    = LOCAL3
{-# LINE 112 "./Syslog.hsc" #-}
  toEnum 160    = LOCAL4
{-# LINE 113 "./Syslog.hsc" #-}
  toEnum 168    = LOCAL5
{-# LINE 114 "./Syslog.hsc" #-}
  toEnum 176    = LOCAL6
{-# LINE 115 "./Syslog.hsc" #-}
  toEnum 184    = LOCAL7
{-# LINE 116 "./Syslog.hsc" #-}
  toEnum i = error ("Syslog.Facility cannot be mapped to value " ++ show i)

  fromEnum KERN      = 0
{-# LINE 119 "./Syslog.hsc" #-}
  fromEnum USER      = 8
{-# LINE 120 "./Syslog.hsc" #-}
  fromEnum MAIL      = 16
{-# LINE 121 "./Syslog.hsc" #-}
  fromEnum DAEMON    = 24
{-# LINE 122 "./Syslog.hsc" #-}
  fromEnum AUTH      = 32
{-# LINE 123 "./Syslog.hsc" #-}
  fromEnum SYSLOG    = 40
{-# LINE 124 "./Syslog.hsc" #-}
  fromEnum LPR       = 48
{-# LINE 125 "./Syslog.hsc" #-}
  fromEnum NEWS      = 56
{-# LINE 126 "./Syslog.hsc" #-}
  fromEnum UUCP      = 64
{-# LINE 127 "./Syslog.hsc" #-}
  fromEnum CRON      = 72
{-# LINE 128 "./Syslog.hsc" #-}
  fromEnum AUTHPRIV  = 80
{-# LINE 129 "./Syslog.hsc" #-}
  fromEnum FTP       = 88
{-# LINE 130 "./Syslog.hsc" #-}
  fromEnum LOCAL0    = 128
{-# LINE 131 "./Syslog.hsc" #-}
  fromEnum LOCAL1    = 136
{-# LINE 132 "./Syslog.hsc" #-}
  fromEnum LOCAL2    = 144
{-# LINE 133 "./Syslog.hsc" #-}
  fromEnum LOCAL3    = 152
{-# LINE 134 "./Syslog.hsc" #-}
  fromEnum LOCAL4    = 160
{-# LINE 135 "./Syslog.hsc" #-}
  fromEnum LOCAL5    = 168
{-# LINE 136 "./Syslog.hsc" #-}
  fromEnum LOCAL6    = 176
{-# LINE 137 "./Syslog.hsc" #-}
  fromEnum LOCAL7    = 184
{-# LINE 138 "./Syslog.hsc" #-}

-- |Options for the syslog service. Set with 'withSyslog'.

data Option
  = PID       -- ^ log the pid with each message
  | CONS      -- ^ log on the console if errors in sending
  | ODELAY    -- ^ delay open until first @syslog()@ (default)
  | NDELAY    -- ^ don't delay open
  | NOWAIT    -- ^ don't wait for console forks: DEPRECATED
  | PERROR    -- ^ log to 'stderr' as well (might be a no-op on some systems)
  deriving (Eq, Bounded, Show)

instance Enum Option where
  toEnum 1     = PID
{-# LINE 152 "./Syslog.hsc" #-}
  toEnum 2    = CONS
{-# LINE 153 "./Syslog.hsc" #-}
  toEnum 4  = ODELAY
{-# LINE 154 "./Syslog.hsc" #-}
  toEnum 8  = NDELAY
{-# LINE 155 "./Syslog.hsc" #-}
  toEnum 16  = NOWAIT
{-# LINE 156 "./Syslog.hsc" #-}
  toEnum 32  = PERROR
{-# LINE 157 "./Syslog.hsc" #-}
  toEnum i = error ("Syslog.Option cannot be mapped to value " ++ show i)

  fromEnum PID     = 1
{-# LINE 160 "./Syslog.hsc" #-}
  fromEnum CONS    = 2
{-# LINE 161 "./Syslog.hsc" #-}
  fromEnum ODELAY  = 4
{-# LINE 162 "./Syslog.hsc" #-}
  fromEnum NDELAY  = 8
{-# LINE 163 "./Syslog.hsc" #-}
  fromEnum NOWAIT  = 16
{-# LINE 164 "./Syslog.hsc" #-}
  fromEnum PERROR  = 32
{-# LINE 165 "./Syslog.hsc" #-}

-- * Haskell API to syslog

-- |Bracket an 'IO' computation between calls to '_openlog'
-- and '_closelog'. Since these settings are for the
-- /process/, multiple calls to this function will,
-- unfortunately, overwrite each other.
--
-- Example:
--
-- > main = withSyslog "my-ident" [PID, PERROR] USER $ do
-- >          putStrLn "huhu"
-- >          syslog Debug "huhu"

withSyslog :: String -> [Option] -> Facility -> IO a -> IO a
withSyslog ident opts facil f = do
  let opt = toEnum . sum . map fromEnum $ opts
  let fac = toEnum . fromEnum           $ facil
  withCString ident $ \p ->
    bracket_ (_openlog p opt fac) (_closelog) f

-- |Log a message with the given priority.

syslog :: Priority -> String -> IO ()
syslog l msg =
  withCString (safeMsg msg)
    (\p -> _syslog (toEnum (fromEnum l)) p)

-- * Helpers

-- | @useSyslog ident@ @=@ @withSyslog ident [PID, PERROR] USER@

useSyslog :: String -> IO a -> IO a
useSyslog ident = withSyslog ident [PID, PERROR] USER

-- |Escape any occurances of \'@%@\' in a string, so that it
-- is safe to pass it to '_syslog'. The 'syslog' wrapper
-- does this automatically.

safeMsg :: String -> String
safeMsg []       = []
safeMsg ('%':xs) = '%' : '%' : safeMsg xs
safeMsg ( x :xs) = x : safeMsg xs

-- * Low-level C functions

foreign import ccall unsafe "closelog" _closelog ::
  IO ()

foreign import ccall unsafe "openlog" _openlog ::
  CString -> CInt -> CInt -> IO ()

foreign import ccall unsafe "setlogmask" _setlogmask ::
  CInt -> IO CInt

foreign import ccall unsafe "syslog" _syslog ::
  CInt -> CString -> IO ()
