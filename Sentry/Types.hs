{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Sentry.Types
-- Copyright   : (c) 2012 Vo Minh Thu,
--
-- License     : BSD-style
-- Maintainer  : thu@hypered.be
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains the different data types and type synonyms used in
-- Sentry.
module Sentry.Types where

import Data.Data (Data)
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
  , eDelay :: Int -- ^ Dealy before re-starting a process, in milliseconds.
  , eCount :: Int -- ^ Number of requested processes of this type.
  , eColor :: Maybe Color
  }
  deriving (Data, Typeable)
  -- Data is only needed so we can have [Entry]
  -- inside the Sentry.Command.Start command.

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
entry :: ProcessType -> String -> [String] -> Int -> Int -> Entry
entry typ cmd args delay count =
  Entry typ cmd args delay count Nothing

data MonitoredEntry = MonitoredEntry
  { mEntry :: Entry -- ^ Process specification.
  , mHandles :: [Int] -- ^ List of process handles running the process
  -- specification (Int is used instead of ProcessHandle so we can
  -- save/restore them with SafeCopy).
  }
  deriving Typeable

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
                    -- has been changed, added, or removed.
  | ProcessExited ProcessType Int -- ^ A process has exited. Its type and its
                                  -- ProcessHandle (as an Int) are given.
  | Reexec -- ^ Re-exec the application, usually after a SIGHUP.
  | Quit -- ^ Request the application to terminate, usually after SIGINT.
