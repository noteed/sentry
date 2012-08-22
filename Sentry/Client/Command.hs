{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Sentry.Client.Command
-- Copyright   : (c) 2012 Vo Minh Thu,
--
-- License     : BSD-style
-- Maintainer  : thu@hypered.be
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides the command-line interface for the Sentry client
-- program. It defines data types to represent different subcommands and the
-- corresponding functions.
module Sentry.Client.Command where

import System.Console.CmdArgs.Implicit

-- | 'client' provides the main function.
client :: IO ()
client = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdScale
    ]
  &= summary versionString
  &= program "sentry"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "Sentry Client - Process monitoring. Copyright (c) 2012 Vo Minh Thu."
  -- TODO add the version.

-- | Data type representing the different command-line subcommands.
data Cmd =
    Scale
    -- ^ Scale a process type (i.e. change the number of workers running
    -- that type).
  deriving (Data, Typeable)

-- | Create a 'Scale' command.
cmdScale :: Cmd
cmdScale = Scale
  &= help ("Instruct Sentry to change the number of workers for a given "
    ++ "process type.")
  &= explicit
  &= name "scale"

-- | Run a Sentry sub-command.
runCmd :: Cmd -> IO ()
runCmd Scale{..} = do
  putStrLn "TODO: sentry scale"
