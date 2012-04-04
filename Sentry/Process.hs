{-# LANGUAGE RecordWildCards #-}
module Sentry.Process where

import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.Time.Clock.POSIX(getPOSIXTime)
import System.Process (createProcess, proc, waitForProcess)

data Process = Process
  { pType :: String
  , pCommand :: String
  , pArguments :: [String]
  , pDelay :: Int -- ^ dealy before re-starting a process, in milliseconds
  }
  deriving (Show, Eq, Ord)

-- | Run a command in its own process.
spawn :: String -> [String] -> IO ()
spawn command args = do
  (_, _, _, h) <- createProcess (proc command args)
  putStrLn $ "`" ++ command ++ "` running"
  exitCode <- waitForProcess h
  putStrLn $ "`" ++ command ++ "` exited with " ++ show exitCode ++ "."

keepSpawned :: Process -> IO ()
keepSpawned Process{..} = forever $ do
  -- If more control is needed when forking the process (e.g. to run some
  -- code before exec'ing the command), the System.Posix.Process module in
  -- the unix package can be used.
  (_, _, _, h) <- createProcess (proc pCommand pArguments)
  t1 <- floor <$> getPOSIXTime
  putStrLn $ "`" ++ pType ++ "` started at " ++ show t1 ++ "."
  exitCode <- waitForProcess h
  t2 <- floor <$> getPOSIXTime
  putStrLn $ "`" ++ pType ++ "` exited at " ++ show t2 ++ " with "
    ++ show exitCode ++ "."
  threadDelay $ pDelay * 1000
