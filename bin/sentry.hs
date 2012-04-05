{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Data.IORef
import Data.Version (showVersion)
import Paths_sentry (version)
import System.Console.CmdArgs.Implicit
import System.Posix.Process (getProcessID)

import Sentry.Process
import Sentry.Types (Process(..), Sentry(..))

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes
    [ cmdStart
    , cmdContinue
    , cmdRespawn -- the important stuff
    ]
  &= summary versionString
  &= program "sentry"

versionString :: String
versionString =
  "Sentry " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

data Cmd =
    Start
  | Continue
  | Respawn
  deriving (Data, Typeable)

cmdStart :: Cmd
cmdStart = Start
  &= help "Start Sentry."
  &= explicit
  &= name "start"

cmdContinue :: Cmd
cmdContinue = Continue
  &= help ("Resume Sentry after a graceful exit or SIGHUP." ++
    " This is normally not called explicitely from the command-line.")
  &= explicit
  &= name "continue"

cmdRespawn :: Cmd
cmdRespawn = Respawn
  &= help "Spawn a command forever (currently hard-coded to `sleep 2`)."
  &= explicit
  &= name "respawn"

processCmd :: Cmd -> IO ()
processCmd Start{..} = do
  state <- initializeState
  stateRef <- newIORef state
  m <- newEmptyMVar
  pid <- fromIntegral <$> getProcessID :: IO Int
  putStrLn $ "Sentry started (PID: " ++ show pid ++ ")."
  waitHUP stateRef m

processCmd Continue{..} = do
  mstate <- readState
  case mstate of
    Nothing -> return ()
    Just state@Sentry{..} -> do
      putStrLn $ "Sentry reexec'd. Initially started at " ++
        show sStartTime ++ " (Previously reexec'd at " ++
        show sReexecTime ++ ")."
      t <- getTime
      stateRef <- newIORef state { sReexecTime = t }
      m <- newEmptyMVar
      waitHUP stateRef m

processCmd Respawn{..} = do
  keepSpawned $ Process "main" "sleep" ["2"] 1000
