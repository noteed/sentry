{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Sentry.Types where

import Data.SafeCopy
import Data.Typeable

-- | Process specification.
data Process = Process
  { pType :: String
  , pCommand :: String
  , pArguments :: [String]
  , pDelay :: Int -- ^ dealy before re-starting a process, in milliseconds
  }
  deriving Typeable

-- | The application state can be serialized and saved to disk then restored
-- when the process is reexec'd.
data Sentry = Sentry
  { sStartTime :: Int -- ^ When the process was started.
  , sReexecTime :: Int -- ^ When the process was reexec'd for the last time. TODO use Maybe Int
  }
  deriving Typeable

$(deriveSafeCopy 0 'base ''Sentry)
