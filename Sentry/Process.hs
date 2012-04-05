{-# LANGUAGE RecordWildCards #-}
module Sentry.Process where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Applicative ((<$>))
import Control.Monad (forever)
import qualified Data.ByteString as B
import Data.IORef
import Data.Serialize (runGet, runPut)
import Data.SafeCopy (safeGet, safePut)
import Data.Time.Clock.POSIX(getPOSIXTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import System.Posix.Files (readSymbolicLink)
import System.Posix.Process (executeFile, getProcessID)
import System.Posix.Signals (installHandler, sigHUP, Handler(..))
import System.Process (createProcess, proc, waitForProcess)

import Sentry.Types (Process(..), Sentry(..))

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
  t1 <- floor <$> getPOSIXTime :: IO Int
  putStrLn $ "`" ++ pType ++ "` started at " ++ show t1 ++ "."
  exitCode <- waitForProcess h
  t2 <- floor <$> getPOSIXTime :: IO Int
  putStrLn $ "`" ++ pType ++ "` exited at " ++ show t2 ++ " with "
    ++ show exitCode ++ "."
  threadDelay $ pDelay * 1000

-- Inspired by the `executale-path` package, which implements
-- a similar function for different OS.
getExecutablePath :: IO FilePath
getExecutablePath = do
  pid <- fromIntegral <$> getProcessID :: IO Int
  path <- readSymbolicLink $ "/proc/" ++ show pid ++ "/exe"
  return path

ensureStateDirectory :: IO ()
ensureStateDirectory = do
  home <- getHomeDirectory
  let dir = home </> ".sentry"
  createDirectoryIfMissing False dir

getStatePath :: IO FilePath
getStatePath = do
  home <- getHomeDirectory
  return $ home </> ".sentry" </> "sentry.state"

reexecute :: Sentry -> IO a
reexecute state = do
  saveState state
  path <- getExecutablePath
  executeFile path False ["continue"] Nothing

-- TODO move in another module
initializeState :: IO Sentry
initializeState = do
  t <- floor <$> getPOSIXTime
  return Sentry
    { sStartTime = t
    , sReexecTime = t -- not really meaningful but doesn't matter
    }

getTime :: IO Int
getTime = floor <$> getPOSIXTime

saveState :: Sentry -> IO ()
saveState state = do
  ensureStateDirectory
  statePath <- getStatePath
  B.writeFile statePath . runPut $ safePut state

readState :: IO (Maybe Sentry)
readState = do
  statePath <- getStatePath
  b <- doesFileExist statePath
  if b
    then do
      content <- B.readFile statePath
      case runGet safeGet content of
        Left err -> do
          putStrLn $ "Can't parse existing state." ++
            " Sentry can't continue. (Error was: " ++ err ++ ".)"
          return Nothing
        Right a -> return $ Just a
    else do
      putStrLn $ "The file `" ++ statePath ++
        "` doesn't exist. Sentry can't continue."
      return Nothing

setupHUP :: MVar () -> IO ()
setupHUP m = do
  _ <- installHandler sigHUP (Catch $ handleHUP m) Nothing
  return ()

handleHUP :: MVar () -> IO ()
handleHUP m = putMVar m ()

-- | `waitHUP` is called in the main thread. It installs a handler for SIGHUP.
-- When the handler is executed (in its own thread), it will set an MVar. The
-- main thread waits for the MVar. When the MVar is available, it means SIGHUP
-- was received and the main thread reexec the program.
waitHUP :: IORef Sentry -> MVar () -> IO ()
waitHUP stateRef m = do
  setupHUP m
  _ <- takeMVar m
  putStrLn "SIGHUP received. Reexec'ing Sentry."
  state <- readIORef stateRef
  reexecute state
