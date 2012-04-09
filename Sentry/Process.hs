{-# LANGUAGE RecordWildCards #-}
module Sentry.Process where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.Maybe (mapMaybe)
import Data.Serialize (runGet, runPut)
import Data.SafeCopy (safeGet, safePut)
import Data.Time.Clock.POSIX(getPOSIXTime)
import System.Console.ANSI (setSGRCode, ColorIntensity(..), Color(..)
  , ConsoleLayer(..), SGR(..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((<.>), (</>), takeDirectory)
import System.IO (hClose, openFile, IOMode(..))
import System.Locale (defaultTimeLocale)
import System.Posix.Files (readSymbolicLink)
import System.Posix.Process (executeFile, getProcessID)
import System.Posix.Signals (installHandler, sigHUP, sigINT, signalProcess
  , Handler(..))
import System.Process (createProcess, getProcessExitCode, proc
  , runProcess, terminateProcess, waitForProcess)
import System.Process.Internals
import System.Time (formatCalendarTime, getClockTime, toCalendarTime)

import Sentry.Types

-- | Create a new process, monitored by its own thread. The provided channel
-- is used by the monitoring thread when the process exits.
spawn :: Chan Command -> Entry -> IO Int -- TODO newtype ProcessID
spawn chan p@Entry{..} = do
  (_, _, _, h) <- createProcess (proc eCommand eArguments)
  i <- processHandleToInt h
  t1 <- getTime
  logP p i $ "Started at " ++ show t1 ++ "."
  _ <- forkIO $ waitProcess chan p h
  return i

-- | Wait for a process to complete. When it happens, it will notify the main
-- thread using the provided channel.
waitProcess :: Chan Command -> Entry -> ProcessHandle -> IO ()
waitProcess chan p@Entry{..} h = do
  i <- processHandleToInt h
  exitCode <- waitForProcess h
  t <- getTime
  logP p i $ "Exited at " ++ show t ++ " with " ++ show exitCode ++ "."
  threadDelay $ eDelay * 1000
  writeChan chan $ ProcessExited eType i

-- | Continue to wait for monitored processes (normally used after the
-- application has re-exec'd itself).
followMonitoredProcess :: Chan Command -> MonitoredEntry -> IO ()
followMonitoredProcess chan MonitoredEntry{..} = do
  mapM_ (follow chan mEntry) mHandles

-- | Continue to wait for a process.
follow :: Chan Command -> Entry -> Int -> IO ()
follow chan p@Entry{..} i = do
  h <- intToProcessHandle i
  -- If it returns Nothing, then we have to continue waiting for the process
  -- (the handle is valid) otherwise the process has completed.
  mCode <- getProcessExitCode h
  case mCode of
    Just _ -> writeChan chan $ ProcessExited eType i
    Nothing -> do
      _ <- forkIO $ waitProcess chan p h
      return ()

-- | Given a monitored process, adjust the number of worker processes to match
-- the possibly updated spec.
updateProcess :: Chan Command -> MonitoredEntry -> IO MonitoredEntry
updateProcess chan p@MonitoredEntry{..} = do
  case length mHandles `compare` eCount mEntry of
    EQ -> return p
    GT -> do
      let n = length mHandles - eCount mEntry
          toTerminate = take n mHandles
          toKeep = drop n mHandles
      -- logP mEntry $ show n ++ " less workers required."
      hs <- mapM intToProcessHandle toTerminate -- TODO make sure eCount always >= 0
      mapM_ terminateProcess hs -- TODO is SIGTERM really what we want?
      return p { mHandles = toKeep }
    LT -> do
      let n = eCount mEntry - length mHandles
      is <- replicateM n $ spawn chan  mEntry
      return p { mHandles = is ++ mHandles }

-- | Remove the process handle from the monitor process (if they match,
-- otherwise do nothing).
exitProcess :: ProcessType -> Int -> MonitoredEntry -> MonitoredEntry
exitProcess typ i p@MonitoredEntry{..} =
  if eType mEntry == typ
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
    Reexec -> do
      -- TODO Compile/Reexec can be splitted:
      -- First start a thread waiting while compiling.
      -- Second push Reexec on the chan.
      -- TODO compile only if the file has been modified.
      b <- compile state
      if b
        then reexecute state
        else processChan state chan
    Quit -> do
      putStrLn "Bye."
      -- TODO SIGTERM should be replaced by SIGINT and a small
      -- waiting period?
      mapM_ (mapM_ (\i -> intToProcessHandle i >>=
        terminateProcess) . mHandles) sProcesses

-- | Given a list of process specifications, start to monitor them.
monitor :: [Entry] -> IO ()
monitor entries = do
  state <- initializeState $ colorize entries
  home <- getHomeDirectory
  let sentry = home </> ".sentry"
      conf = sentry </> "conf"
      binPath = sExecutablePath state
      pidPath = binPath <.> "pid"

  pid <- fromIntegral <$> getProcessID :: IO Int
  if takeDirectory binPath /= conf
    then putStrLn $ "Sentry started (PID: " ++ show pid ++ ")."
    else do
      putStrLn $ "Sentry started (PID: " ++ show pid ++ "saved in "
        ++ pidPath ++ ")."
      writeFile pidPath $ show pid

  chan <- newChan
  setupHUP chan
  setupINT chan
  writeChan chan UpdateProcesses
  processChan state chan

-- | Given a restored application state, and the new process specifications,
-- continue the monitoring.
continueMonitor :: Sentry -> [Entry] -> IO ()
continueMonitor state entries = do
  chan <- newChan
  setupHUP chan
  setupINT chan
  ps <- mapM (continueProcess $ colorize entries) (sProcesses state)
  let state' = state { sProcesses = mapMaybe id ps }
  mapM_ (followMonitoredProcess chan) (sProcesses state')
  writeChan chan UpdateProcesses
  processChan state' chan

-- TODO newly added specifications are not actually added.
continueProcess :: [Entry] -> MonitoredEntry -> IO (Maybe MonitoredEntry)
continueProcess entries MonitoredEntry{..} = do
  case lookupProcess mEntry entries of
    Just p -> return . Just $ MonitoredEntry p mHandles
    Nothing -> do
      putStrLn $ "Process type `" ++ eType mEntry ++ "` removed. Killing "
        ++ "workers."
      mapM_ (\i -> intToProcessHandle i >>= terminateProcess) mHandles
      return Nothing

-- | Try to find a matching process in the given list.
lookupProcess :: Entry -> [Entry] -> Maybe Entry
lookupProcess p entries =
  case filter (sameEntries p) entries of
    [p'] -> Just p'
    [] -> Nothing
    _ -> error "More than one process"
    -- TODO `entries` must be a Map instead of a list.

-- | Compare if two entries are equal, i.e. if they have
-- same type, command and arguments.
sameEntries :: Entry -> Entry -> Bool
sameEntries p1 p2 = eType p1 == eType p2
  && eCommand p1 == eCommand p2
  && eArguments p1 == eArguments p2

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
      conf = dir </> "conf"
  createDirectoryIfMissing False dir
  createDirectoryIfMissing False conf

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

-- | Compile itself.
compile :: Sentry -> IO Bool
compile state = do
  home <- getHomeDirectory
  let sentry = home </> ".sentry"
      conf = sentry </> "conf"
      binPath = sExecutablePath state
      sourcePath = binPath <.> "hs"
      errorPath = binPath <.> "error"

  if takeDirectory binPath /= conf
    then do
      putStrLn $ binPath ++ " is not under " ++ conf ++ "."
      return False
    else do
      status <- bracket (openFile errorPath WriteMode) hClose $ \h -> do
        p <- runProcess "ghc"
          [ "--make", sourcePath, "-fforce-recomp", "-v0", "-threaded",
            "-o", binPath]
          (Just conf)
          Nothing Nothing Nothing (Just h)
        waitForProcess p

      if status == ExitSuccess
        then do
          putStrLn $ sourcePath ++ " successfully compiled."
          return True
        else do
          content <- readFile errorPath
          putStrLn $ "Problem encountered while compiling " ++ sourcePath ++ ":"
          putStrLn content
          return False

sendSIGHUP :: Sentry -> IO ()
sendSIGHUP state = do
  home <- getHomeDirectory
  let binPath = sExecutablePath state
      pidPath = binPath <.> "pid"
      -- TODO all these xxxPath could be function on Sentry.
  -- TODO better error messages.
  content <- readFile pidPath
  signalProcess sigHUP $ fromIntegral $ (read content :: Int)

-- TODO move in another module
-- | Create a initial application state from a list of process specifications.
initializeState :: [Entry] -> IO Sentry
initializeState specs = do
  path <- getExecutablePath
  t <- getTime
  return Sentry
    { sExecutablePath = path
    , sStartTime = t
    , sReexecTime = t -- not really meaningful but doesn't matter
    , sProcesses = map (flip MonitoredEntry []) specs
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

colorize :: [Entry] -> [Entry]
colorize entries = zipWith f entries $ cycle $ map Just
  [Red, Blue, Green, Yellow, Magenta, Cyan, White]
  where f p c = p { eColor = c }

colorized :: Entry -> String -> String
colorized Entry{..} str =
  case eColor of
    Nothing -> pad str
    Just c ->
      setSGRCode [SetColor Foreground Dull c] ++  pad str ++ setSGRCode []

pad :: String -> String
pad s = s ++ replicate (25 - length s) ' '

logP :: Entry -> Int -> String -> IO ()
logP p@Entry{..} i s = do
  ts <- getTimeString
  putStrLn $ colorized p (ts ++ " " ++ eType ++ "." ++ show i) ++ s

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

getTimeString :: IO String
getTimeString = do
  tm <- getClockTime
  ct <- toCalendarTime tm
  return $ formatCalendarTime defaultTimeLocale "%H:%M:%S" ct
