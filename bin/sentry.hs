{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative ((<$>))
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
    ]
  &= summary versionString
  &= program "sentry"

versionString :: String
versionString =
  "Sentry " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

data Cmd =
    Start
  | Continue
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

processCmd :: Cmd -> IO ()
processCmd Start{..} = do
  pid <- fromIntegral <$> getProcessID :: IO Int
  putStrLn $ "Sentry started (PID: " ++ show pid ++ ")."
  monitor processes

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
      continueMonitor state'

processes :: [Process]
processes =
  [ Process "short" "sleep" ["2"] 1000 1
  , Process "long" "sleep" ["10"] 1000 1
  ]
