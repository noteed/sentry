{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Sentry.Server.Types
-- Copyright   : (c) 2012 Vo Minh Thu,
--
-- License     : BSD-style
-- Maintainer  : thu@hypered.be
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains the different data types and type synonyms used in
-- Sentry.
module Sentry.Server.Types where

import Data.Data (Data)
import Data.List (intercalate)
import Data.SafeCopy
import Data.Typeable
import System.Console.ANSI (Color(..))

-- | A process type is just a name for a specific command.
type ProcessType = String

-- | An entry in the configuration, i.e. a process specification.
data Entry = Entry
  { eType :: ProcessType -- ^ Process type.
  , eCommand :: String -- ^ Command.
  , eArguments :: [String] -- ^ Command arguments.
  , eDelay :: Int -- ^ Delay before re-starting a process, in milliseconds.
  , eCount :: Int -- ^ Number of requested processes of this type.
  , ePort :: Maybe (String, Int)
    -- ^ Port to listen on, with the command's option to provide it.
  }
  deriving (Data, Typeable)
  -- Data is only needed so we can have [Entry]
  -- inside the Sentry.Server.Command.Start command.

showEntry :: Entry -> String
showEntry Entry{..} = show eCount ++ " " ++ eType ++ ": " ++ eCommand ++ " "
  ++ intercalate " " (eArguments ++ maybe [] port ePort)
  ++ "  -- " ++ show eDelay ++ "ms."
  where port (option, n) = ["--" ++ option, show n]

deriving instance Data Color
deriving instance Typeable Color
deriveSafeCopy 0 'base ''Color

-- | 'entry' is used to define configuration entries to be passed to 'sentry'.
-- A sample entry looks like
--
-- > entry "dummy" "sleep" ["3"] 1000 1
--
-- It creates an entry with type \"dummy\". The type is an arbitrary string that
-- will appear in the logs. It is also used to dynamically change the
-- configuration by refering to its type.  It then specifies that the command
-- `sleep 3` will be kept running, restarting it after 1000 milliseconds if
-- necessary. The last value is the number of instances to run, in the example
-- just one.
entry :: ProcessType
      -> String
      -> [String]
      -> Int
      -> Int
      -> Maybe (String, Int)
      -> Entry
entry = Entry

-- | Return the command to run a process specification. If a port is needed,
-- it is assumed the file descriptor is available.
command :: MonitoredEntry -> (String, [String])
command MonitoredEntry{..} = case (ePort mEntry, mFD) of
  (Nothing, Nothing) ->
      (eCommand mEntry, eArguments mEntry)
  (Just (opt, port), Just fd) ->
      (eCommand mEntry, eArguments mEntry ++ ["--fd", show fd])
    -- TODO the opt is ignored, so don't provide it in the spec?
  _ -> error
    "Mismatch between process specification and monitored entry state."

data MonitoredEntry = MonitoredEntry
  { mEntry :: Entry -- ^ Process specification.
  , mHandles :: [Int]
  -- ^ List of process handles running the process specification
  -- (Int is used instead of ProcessHandle so we can save/restore
  -- them with SafeCopy).
  , mColor :: Color
  -- ^ Assigned color for logging info about this process
  -- specification to the terminal.
  , mFD :: Maybe Int
  -- ^ Whether the port specified in `mEntry` was already open
  -- by sentry or not. If it is open, this holds the file descriptor.
  }
  deriving Typeable

-- | Helper function to turn an entry into a monitored entry.
monitored :: Entry -> MonitoredEntry
monitored e = MonitoredEntry e [] White Nothing

-- | The application state can be serialized and saved to disk then restored
-- when the process is reexec'd.
data Sentry = Sentry
  { sExecutablePath :: FilePath -- ^ Original executable path.
  , sStartTime :: Int -- ^ When the process was started.
  , sReexecTime :: Maybe Int -- ^ When the process was reexec'd for the last time.
  , sProcesses :: [MonitoredEntry] -- ^ List of monitored processes.
  }
  deriving Typeable

deriveSafeCopy 0 'base ''Entry
deriveSafeCopy 0 'base ''MonitoredEntry
deriveSafeCopy 0 'base ''Sentry

-- | The possible commands the main Sentry thread can execute.
data Command =
    UpdateProcesses -- ^ Request to update the monitored processes list (e.g.
                    -- because a process has exited or a process specification
                    -- has been changed, added, or removed).
  | ProcessExited ProcessType Int -- ^ A process has exited. Its type and its
                                  -- ProcessHandle (as an Int) are given.
  | Reexec -- ^ Re-exec the application, usually after a SIGHUP.
  | Quit -- ^ Request the application to terminate, usually after SIGINT.
  | Scale ProcessType Int -- ^ Instruct Sentry to change the number of running
                          -- processes for a given type.
