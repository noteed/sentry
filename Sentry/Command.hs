{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Sentry.Command where

import Control.Applicative ((<$>))
import System.Console.CmdArgs.Implicit
import System.Posix.Process (getProcessID)

import Sentry.Process
import Sentry.Types (Entry(..), Sentry(..))

sentry :: [Entry] -> IO ()
sentry entries = (processCmd =<<) $ cmdArgs $
  modes
    [ cmdStart entries
    , cmdContinue entries
    , cmdCompile
    ]
  &= summary versionString
  &= program "sentry"

versionString :: String
versionString =
  "Sentry - Process monitoring. Copyright (c) 2012 Vo Minh Thu."
  -- TODO add the version.

data Cmd =
    Start { cmdEntries :: [Entry] }
  | Continue { cmdEntries :: [Entry] }
  | Compile
  deriving (Data, Typeable)

cmdStart :: [Entry] -> Cmd
cmdStart entries = Start
  { cmdEntries = entries
    &= ignore
  } &= help "Start Sentry."
    &= explicit
    &= name "start"

cmdContinue :: [Entry] -> Cmd
cmdContinue entries = Continue
  { cmdEntries = entries
    &= ignore
  } &= help ("Resume Sentry after a graceful exit or SIGHUP." ++
      " This is normally not called explicitely from the command-line.")
    &= explicit
    &= name "continue"

cmdCompile :: Cmd
cmdCompile = Compile
  &= help "Compile the configuration file, replacing this executable."
  &= explicit
  &= name "compile"

processCmd :: Cmd -> IO ()
processCmd Start{..} = do
  pid <- fromIntegral <$> getProcessID :: IO Int
  putStrLn $ "Sentry started (PID: " ++ show pid ++ ")."
  monitor cmdEntries

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
      continueMonitor state' cmdEntries

processCmd Compile{..} = do
  state <- initializeState []
  _ <- compile state
  return ()
