{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Sentry.Types where

import Data.Data (Data)
import Data.SafeCopy
import Data.Typeable
import System.Console.ANSI (Color(..))

-- | A process type is just a name for a specific command.
type ProcessType = String

-- | Process specification.
data Process = Process
  { pType :: ProcessType -- ^ Process type.
  , pCommand :: String -- ^ Command.
  , pArguments :: [String] -- ^ Command arguments.
  , pDelay :: Int -- ^ Dealy before re-starting a process, in milliseconds.
  , pCount :: Int -- ^ Number of requested processes of this type.
  , pColor :: Maybe Color
  }
  deriving (Data, Typeable)
  -- Data is only needed so we can have [Process]
  -- inside the Sentry.Command.Start command.

deriving instance Data Color
deriving instance Typeable Color
deriveSafeCopy 0 'base ''Color

-- | Simple helper to specify a process type.
process :: ProcessType -> String -> [String] -> Int -> Int -> Process
process typ cmd args delay count =
  Process typ cmd args delay count Nothing

data MonitoredProcess = MonitoredProcess
  { mProcess :: Process -- ^ Process specification.
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
  , sReexecTime :: Int -- ^ When the process was reexec'd for the last time. TODO use Maybe Int
  , sProcesses :: [MonitoredProcess] -- ^ List of monitored processes.
  }
  deriving Typeable

deriveSafeCopy 0 'base ''Process
deriveSafeCopy 0 'base ''MonitoredProcess
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
