{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Sample program that sends a signal to the given PID after N calls.
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import System.Console.CmdArgs.Implicit
import System.Posix.Signals (sigINT, signalProcess)

----------------------------------------------------------------------
-- State
----------------------------------------------------------------------

data KillState = KillState
  { sN :: Int -- ^ Number of calls.
  , sSIG :: Int -- ^ The signal to send.
  , sPID :: Int -- ^ The process ID the signal should be sent to.
  }
  deriving Typeable

data MKS = MKS (Maybe KillState)
  deriving Typeable

deriveSafeCopy 0 'base ''KillState
deriveSafeCopy 0 'base ''MKS

writeState :: MKS -> Update MKS ()
writeState = put

queryState :: Query MKS MKS
queryState = ask

makeAcidic ''MKS
  [ 'writeState
  , 'queryState
  ]

openState :: IO (AcidState MKS)
openState = openLocalState (MKS Nothing)

closeState :: AcidState MKS -> IO ()
closeState = closeAcidState

-- | Return any previously saved state.
getState :: IO (Maybe KillState)
getState = do
  a <- openState
  MKS s <- query a QueryState
  closeState a
  return s

-- | If the PID and SIG are the same as previously saved, this increment the
-- call counter. Otherwise, the counter is put back to 1.
updateState :: Int -> Int -> IO KillState
updateState sig pid = do
  a <- openState
  MKS s <- query a QueryState
  let n = case s of
            Just (KillState{..}) | (sig, pid) == (sSIG, sPID) -> sN + 1
            _ -> 1
  _ <- update a $ WriteState (MKS . Just $ KillState n sig pid)
  closeState a
  return $ KillState n sig pid

-- | Remove any previously saved state.
resetState :: IO ()
resetState = do
  a <- openState
  _ <- update a $ WriteState (MKS Nothing)
  closeState a

----------------------------------------------------------------------
-- Command-line
----------------------------------------------------------------------

main :: IO ()
main = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdKill
    , cmdReport
    ]
  &= summary versionString
  &= program "sentry-kill"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "Sentry Kill - Process monitoring. Copyright (c) 2012 Vo Minh Thu."
  -- TODO add the version.

-- | Data type representing the different command-line subcommands.
data Cmd =
    Kill { cmdSIG :: Int, cmdPID :: Int, cmdN :: Int, cmdSeconds :: Int }
    -- ^ Send a signal to a process after N calls.
  | Report
    -- ^ Report the last used arguments for the `kill` sub-command.
  deriving (Data, Typeable)

-- | Create a 'Kill' command.
cmdKill :: Cmd
cmdKill = Kill
  { cmdSIG = def
    &= explicit
    &= name "sig"
  , cmdPID = def
    &= explicit
    &= name "pid"
  , cmdN = def
    &= explicit
    &= name "n"
  , cmdSeconds = def
    &= explicit
    &= name "seconds"
  } &= help "Send a signal to a process after N calls."
    &= explicit
    &= name "kill"

-- | Create a 'Report' command.
cmdReport :: Cmd
cmdReport = Report
    &= help "Report the last used arguments for the `kill` sub-command."
    &= explicit
    &= name "report"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd Kill{..} = do
  KillState{..} <- updateState cmdSIG cmdPID
  threadDelay $ cmdSeconds * 1000 * 1000
  when (cmdN == sN) $ do
    resetState
    putStrLn $ "Killing " ++ show cmdPID ++ "."
    signalProcess sigINT $ fromIntegral cmdPID -- TODO cmdSIG

runCmd Report{..} = do
  ms <- getState
  case ms of
    Just (KillState{..}) ->
      putStrLn $ "PID SIG N: " ++ unwords (map show [sPID, sSIG, sN])
    Nothing -> putStrLn "No previously saved state."
