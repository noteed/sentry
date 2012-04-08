{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Sentry.Command where

import Control.Applicative ((<$>))
import System.Console.CmdArgs.Implicit
import System.Posix.Process (getProcessID)

import Sentry.Process
import Sentry.Types (Process(..), Sentry(..))

sentry :: [Process] -> IO ()
sentry processes = (processCmd =<<) $ cmdArgs $
  modes
    [ cmdStart processes
    , cmdContinue processes
    ]
  &= summary versionString
  &= program "sentry"

versionString :: String
versionString =
  "Sentry - Process monitoring. Copyright (c) 2012 Vo Minh Thu."
  -- TODO add the version.

data Cmd =
    Start { cmdProcesses :: [Process] }
  | Continue { cmdProcesses :: [Process] }
  deriving (Data, Typeable)

cmdStart :: [Process] -> Cmd
cmdStart processes = Start
  { cmdProcesses = processes
    &= ignore
  } &= help "Start Sentry."
    &= explicit
    &= name "start"

cmdContinue :: [Process] -> Cmd
cmdContinue processes = Continue
  { cmdProcesses = processes
    &= ignore
  } &= help ("Resume Sentry after a graceful exit or SIGHUP." ++
      " This is normally not called explicitely from the command-line.")
    &= explicit
    &= name "continue"

processCmd :: Cmd -> IO ()
processCmd Start{..} = do
  pid <- fromIntegral <$> getProcessID :: IO Int
  putStrLn $ "Sentry started (PID: " ++ show pid ++ ")."
  monitor cmdProcesses

processCmd Continue{..} = do
  mstate <- readState
  case mstate of
    Nothing -> return ()
    Just state@Sentry{..} -> do
      putStrLn $ "Sentry reexec'd. Initially started at " ++
        show sStartTime ++ " (Previously reexec'd at " ++
        show sReexecTime ++ ")."
      t <- getTime
      let state' = state { sReexecTime = t }
      continueMonitor state' cmdProcesses
