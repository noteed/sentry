{-# LANGUAGE RecordWildCards #-}
module Sentry.Process where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.Maybe (mapMaybe)
import Data.Serialize (runGet, runPut)
import Data.SafeCopy (safeGet, safePut)
import Data.Time.Clock.POSIX(getPOSIXTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import System.Posix.Files (readSymbolicLink)
import System.Posix.Process (executeFile, getProcessID)
import System.Posix.Signals (installHandler, sigHUP, sigINT, Handler(..))
import System.Process (createProcess, getProcessExitCode, proc
  , terminateProcess, waitForProcess)
import System.Process.Internals

import Sentry.Types

-- | Create a new process, monitored by its own thread. The provided channel
-- is used by the monitoring thread when the process exits.
spawn :: Chan Command -> Process -> IO Int -- TODO newtype ProcessID
spawn chan p@Process{..} = do
  (_, _, _, h) <- createProcess (proc pCommand pArguments)
  i <- processHandleToInt h
  t1 <- getTime
  putStrLn $ "`" ++ pType ++ "` started at " ++ show t1 ++ "."
  _ <- forkIO $ waitProcess chan p h
  return i

-- | Wait for a process to complete. When it happens, it will notify the main
-- thread using the provided channel.
waitProcess :: Chan Command -> Process -> ProcessHandle -> IO ()
waitProcess chan Process{..} h = do
  i <- processHandleToInt h
  exitCode <- waitForProcess h
  t <- getTime
  putStrLn $ "`" ++ pType ++ "` exited at " ++ show t ++ " with "
    ++ show exitCode ++ "."
  threadDelay $ pDelay * 1000
  writeChan chan $ ProcessExited pType i

-- | Continue to wait for monitored processes (normally used after the
-- application has re-exec'd itself).
followMonitoredProcess :: Chan Command -> MonitoredProcess -> IO ()
followMonitoredProcess chan MonitoredProcess{..} = do
  mapM_ (follow chan mProcess) mHandles

-- | Continue to wait for a process.
follow :: Chan Command -> Process -> Int -> IO ()
follow chan p@Process{..} i = do
  h <- intToProcessHandle i
  -- If it returns Nothing, then we have to continue waiting for the process
  -- (the handle is valid) otherwise the process has completed.
  mCode <- getProcessExitCode h
  case mCode of
    Just _ -> writeChan chan $ ProcessExited pType i
    Nothing -> do
      _ <- forkIO $ waitProcess chan p h
      return ()

-- | Given a monitored process, adjust the number of worker processes to match
-- the possibly updated spec.
updateProcess :: Chan Command -> MonitoredProcess -> IO MonitoredProcess
updateProcess chan p@MonitoredProcess{..} = do
  case length mHandles `compare` pCount mProcess of
    EQ -> return p 
    GT -> do
      let n = length mHandles - pCount mProcess
          toTerminate = take n mHandles
          toKeep = drop n mHandles
      putStrLn $ show n ++ " less workers required for process type `"
        ++ pType mProcess ++ "`."
      hs <- mapM intToProcessHandle toTerminate -- TODO make sure pCount always >= 0
      mapM_ terminateProcess hs -- TODO is SIGTERM really what we want?
      return p { mHandles = toKeep }
    LT -> do
      let n = pCount mProcess - length mHandles
      putStrLn $ show n ++ " workers required for process type `"
        ++ pType mProcess ++ "`."
      is <- replicateM n $ spawn chan  mProcess
      return p { mHandles = is ++ mHandles }

-- | Remove the process handle from the monitor process (if they match,
-- otherwise do nothing).
exitProcess :: ProcessType -> Int -> MonitoredProcess -> MonitoredProcess
exitProcess typ i p@MonitoredProcess{..} =
  if pType mProcess == typ
  then p { mHandles = filter (/= i) mHandles }
  else p

-- | Process the command received on the main channel. This acts as a main
-- event loop.
processChan :: Sentry -> Chan Command -> IO ()
processChan state@Sentry{..} chan = do
  command <- readChan chan
  case command of
    UpdateProcesses -> do
      ps <- mapM (updateProcess chan) sProcesses
      let state' = state { sProcesses = ps }
      processChan state' chan
    ProcessExited typ i -> do
      let ps = map (exitProcess typ i) sProcesses
      ps' <- mapM (updateProcess chan) ps
      let state' = state { sProcesses = ps' }
      processChan state' chan
    Reexec -> reexecute state
    Quit -> do
      putStrLn "Bye."
      -- TODO SIGTERM should be replaced by SIGINT and a small
      -- waiting period?
      mapM_ (mapM_ (\i -> intToProcessHandle i >>=
        terminateProcess) . mHandles) sProcesses

-- | Given a list of process specifications, start to monitor them.
monitor :: [Process] -> IO ()
monitor processes = do
  state <- initializeState processes
  chan <- newChan
  setupHUP chan
  setupINT chan
  writeChan chan UpdateProcesses
  processChan state chan

-- | Given a restored application state, and the new process specifications,
-- continue the monitoring.
continueMonitor :: Sentry -> [Process] -> IO ()
continueMonitor state processes = do
  chan <- newChan
  setupHUP chan
  setupINT chan
  ps <- mapM (continueProcess processes) (sProcesses state)
  let state' = state { sProcesses = mapMaybe id ps }
  mapM_ (followMonitoredProcess chan) (sProcesses state')
  writeChan chan UpdateProcesses
  processChan state' chan

continueProcess :: [Process] -> MonitoredProcess -> IO (Maybe MonitoredProcess)
continueProcess processes MonitoredProcess{..} = do
  case lookupProcess mProcess processes of
    Just p -> return . Just $ MonitoredProcess p mHandles
    Nothing -> do
      putStrLn $ "Process type `" ++ pType mProcess ++ "` removed. Killing "
        ++ "workers."
      mapM_ (\i -> intToProcessHandle i >>= terminateProcess) mHandles
      return Nothing

-- | Try to find a matching process in the given list.
lookupProcess :: Process -> [Process] -> Maybe Process
lookupProcess process processes =
  case filter (sameProcesses process) processes of
    [p] -> Just p
    [] -> Nothing
    _ -> error "More than one process"
    -- TODO `processes` must be a Map instead of a list.

-- | Compare if two processes are equal, i.e. if they have
-- same type, command and arguments.
sameProcesses :: Process -> Process -> Bool
sameProcesses p1 p2 = pType p1 == pType p2
  && pCommand p1 == pCommand p2
  && pArguments p1 == pArguments p2

-- Inspired by the `executale-path` package, which implements
-- a similar function for different OS.
getExecutablePath :: IO FilePath
getExecutablePath = do
  pid <- fromIntegral <$> getProcessID :: IO Int
  path <- readSymbolicLink $ "/proc/" ++ show pid ++ "/exe"
  return path

-- | Make sure the directory where the application state is saved exists.
ensureStateDirectory :: IO ()
ensureStateDirectory = do
  home <- getHomeDirectory
  let dir = home </> ".sentry"
  createDirectoryIfMissing False dir

-- | Return the path where to save the application state.
getStatePath :: IO FilePath
getStatePath = do
  home <- getHomeDirectory
  return $ home </> ".sentry" </> "sentry.state"

-- | Save the application state then re-exec itself (calling `sentry continue`).
reexecute :: Sentry -> IO a
reexecute state = do
  saveState state
  executeFile (sExecutablePath state) False ["continue"] Nothing

-- TODO move in another module
-- | Create a initial application state from a list of process specifications.
initializeState :: [Process] -> IO Sentry
initializeState specs = do
  path <- getExecutablePath
  t <- getTime
  return Sentry
    { sExecutablePath = path
    , sStartTime = t
    , sReexecTime = t -- not really meaningful but doesn't matter
    , sProcesses = map (flip MonitoredProcess []) specs
    }

-- | Save the application state to disk (normally done just before re-exec'ing
-- itself).
saveState :: Sentry -> IO ()
saveState state = do
  ensureStateDirectory
  statePath <- getStatePath
  B.writeFile statePath . runPut $ safePut state

-- | Try to restore the application state (normally saved previously before
-- re-exec'ing itself).
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

-- | Install the handleHUP function as a SIGHUP handler.
setupHUP :: Chan Command -> IO ()
setupHUP chan = do
  _ <- installHandler sigHUP (Catch $ handleHUP chan) Nothing
  return ()

-- | SIGHUP handler. When the handler is run, it simply instructs the main
-- thread to re-exec the program.
handleHUP :: Chan Command -> IO ()
handleHUP = flip writeChan Reexec

-- | Install the handleINT function as a SIGINT handler.
setupINT :: Chan Command -> IO ()
setupINT chan = do
  _ <- installHandler sigINT (Catch $ handleINT chan) Nothing
  return ()

-- | SIGINT handler. When the handler is run, it simply instructs the main
-- thread to quit the program.
handleINT :: Chan Command -> IO ()
handleINT = flip writeChan Quit

-- | Convenience function to turn a ProcessHandle into an Int (used later when
-- saving/restoring the state with SafeCopy).
processHandleToInt :: ProcessHandle -> IO Int
processHandleToInt (ProcessHandle mvar) = do
  OpenHandle i <- readMVar mvar
  return $ fromIntegral i

-- | Convenience function to turn an Int into a ProcessHandle (used later when
-- saving/restoring the state with SafeCopy).
intToProcessHandle :: Int -> IO ProcessHandle
intToProcessHandle i = do
  mvar <- newMVar $ OpenHandle $ fromIntegral i
  return $ ProcessHandle mvar

-- | Convenience function to get a Posix time as an Int.
getTime :: IO Int
getTime = floor <$> getPOSIXTime
