{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Sentry.Server.Core
-- Copyright   : (c) 2012 Vo Minh Thu,
--
-- License     : BSD-style
-- Maintainer  : thu@hypered.be
-- Stability   : experimental
-- Portability : GHC
--
-- This module is Sentry's core implementation: state initialization, process
-- spawning and monitoring.
module Sentry.Server.Core
  (
  -- * Processes
    spawn
  , follow
  , terminate
  , updateProcess
  , removeProcess
  -- * Application state
  , initializeState
  , saveState
  , readState
  -- * Main entry points
  , startMonitor
  , continueMonitor
  , sendSIGHUP
  , compile
  , reexecute
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Applicative ((<$>))
import Control.Monad (forM_, replicateM, when)
import qualified Data.ByteString as B
import Data.Either (partitionEithers)
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Serialize (runGet, runPut)
import Data.SafeCopy (safeGet, safePut)
import Data.Time.Clock.POSIX(getPOSIXTime)
import System.Console.ANSI (setSGRCode, ColorIntensity(..), Color(..)
  , ConsoleLayer(..), SGR(..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((<.>), (</>), takeDirectory)
import System.IO (hGetLine, hIsEOF, hPutStrLn, hReady, hSetBuffering
  , stderr, stdout, withFile
  , BufferMode(..), IOMode(..), Handle)
import System.Locale (defaultTimeLocale)
import System.Posix.Files (readSymbolicLink)
import System.Posix.Process (executeFile, getProcessID)
import System.Posix.Signals (installHandler, sigHUP, sigINT, signalProcess
  , Handler(..))
import System.Process (createProcess, getProcessExitCode, proc
  , runProcess, terminateProcess, waitForProcess)
import System.Process.Internals
import System.Time (formatCalendarTime, getClockTime, toCalendarTime)

import Sentry.Server.Types

-- | Create a new process, monitored by a new thread. The provided channel
-- is used by the monitoring thread when the process exits to notify the
-- main thread.
spawn :: Chan Command -> Entry -> IO Int -- TODO newtype ProcessID
spawn chan e@Entry{..} = do
  (_, Just hout, Just herr, h) <- createProcess $
    (proc eCommand eArguments)
    { std_out = CreatePipe, std_err = CreatePipe }
  i <- processHandleToInt h
  t1 <- getTime
  logP e i $ "Started at " ++ show t1 ++ "."
  follow chan e i
  _ <- forkIO $ pipeToStdout e i hout herr
  return i

-- | Wait for a process to complete. When it happens, it will notify the main
-- thread using the provided channel. Return without blocking.
follow :: Chan Command -> Entry -> Int -> IO ()
follow chan p@Entry{..} i = do
  _ <- forkIO $ do
    -- After a re-exec, waitForProcess will make an error if the
    -- child has already exited, so use getProcessExitCode at first.
    -- If it returns Nothing, then we have to continue waiting for t
    -- (the handle is valid) otherwise the process has completed.
    h <- intToProcessHandle i
    mCode <- getProcessExitCode h
    case mCode of
      Just _ -> writeChan chan $ ProcessExited eType i
      Nothing -> do
        exitCode <- waitForProcess h
        t <- getTime
        logP p i $ "Exited at " ++ show t ++ " with " ++ show exitCode ++ "."
        threadDelay $ eDelay * 1000
        writeChan chan $ ProcessExited eType i
  return ()

terminate :: MonitoredEntry -> IO ()
terminate MonitoredEntry{..} = do
  putStrLn $ "Process type `" ++ eType mEntry ++ "` removed. Killing "
    ++ "workers."
  mapM_ (\i -> intToProcessHandle i >>= terminateProcess) mHandles

-- | Given a monitored process, adjust the number of worker processes to match
-- the possibly updated spec.
updateProcess :: Chan Command -> MonitoredEntry -> IO MonitoredEntry
updateProcess chan p@MonitoredEntry{..} =
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
      is <- replicateM n $ spawn chan mEntry
      return p { mHandles = is ++ mHandles }

-- | Remove the process handle from the monitored process (if they match,
-- otherwise do nothing).
removeProcess :: ProcessType -> Int -> MonitoredEntry -> MonitoredEntry
removeProcess typ i p@MonitoredEntry{..} =
  if eType mEntry == typ
  then p { mHandles = filter (/= i) mHandles }
  else p

-- | Change the number of requested processes (if they match,
-- otherwise do nothing).
scaleProcess :: ProcessType -> Int -> MonitoredEntry -> MonitoredEntry
scaleProcess typ n p@MonitoredEntry{..} =
  if eType mEntry == typ
  then p { mEntry = mEntry { eCount = n } }
  else p

-- | Given a list of process specifications, start to monitor them. The given
-- MVar is used to report back the evolving state.
startMonitor :: Sentry -> MVar Sentry -> Chan Command -> IO ()
startMonitor state stateVar chan = do
  home <- getHomeDirectory
  let sentry = home </> ".sentryd"
      conf = sentry </> "conf"
      binPath = sExecutablePath state
      pidPath = binPath <.> "pid"

  pid <- fromIntegral <$> getProcessID :: IO Int
  if takeDirectory binPath /= conf
    then putStrLn $ "Sentry started (PID: " ++ show pid ++ ")."
    else do
      putStrLn $ "Sentry started (PID: " ++ show pid ++ " saved in "
        ++ pidPath ++ ")."
      writeFile pidPath $ show pid

  monitor state stateVar chan

-- | Given new process specifications, continue the monitoring. The given MVar
-- is used to report back the evolving state.
continueMonitor :: Sentry -> [Entry] -> MVar Sentry -> Chan Command -> IO ()
continueMonitor state@Sentry{..} entries stateVar chan = do
  putStrLn $ "Sentry reexec'd. Initially started at " ++
    show sStartTime ++ maybe "." (\r -> " (Previously reexec'd at " ++
    show r ++ ").") sReexecTime
  t <- getTime
  let (walkingDeads, kept) = partitionEithers $
        map (continueProcess entries) sProcesses
      state' = state
        { sProcesses = addEntries kept entries
        , sReexecTime = Just t }
  mapM_ terminate walkingDeads
  monitor state' stateVar chan

-- | Monitor the processes from the given application state.
monitor :: Sentry -> MVar Sentry -> Chan Command -> IO ()
monitor state stateVar chan = do
  let state' = state { sProcesses = colorize $ sProcesses state }
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  setupHUP chan
  setupINT chan
  -- follow existing handles, if any (after a re-exec there can be some).
  forM_ (sProcesses state') $
    \p -> mapM_ (follow chan $ mEntry p) $ mHandles p
  writeChan chan UpdateProcesses
  processChan state' chan stateVar

-- | Process the command received on the main channel. This acts as a main
-- event loop.
processChan :: Sentry -> Chan Command -> MVar Sentry -> IO ()
processChan state@Sentry{..} chan stateVar = do
  modifyMVar_ stateVar $ return . const state
  command <- readChan chan
  case command of
    UpdateProcesses -> do
      ps <- mapM (updateProcess chan) sProcesses
      let state' = state { sProcesses = ps }
      processChan state' chan stateVar
    ProcessExited typ i -> do
      let ps = map (removeProcess typ i) sProcesses
      ps' <- mapM (updateProcess chan) ps
      let state' = state { sProcesses = ps' }
      processChan state' chan stateVar
    Reexec -> do
      -- TODO Compile/Reexec can be splitted:
      -- First start a thread waiting while compiling.
      -- Second push Reexec on the chan.
      -- Or simply let the `sentryd reload` command do it
      -- before issuing the SIGHUP signal.
      -- TODO compile only if the file has been modified.
      b <- compile state
      if b
        then reexecute state
        else processChan state chan stateVar
    Quit -> do
      putStrLn "Bye."
      -- TODO SIGTERM should be replaced by SIGINT and a small
      -- waiting period?
      mapM_ (mapM_ (\i -> intToProcessHandle i >>=
        terminateProcess) . mHandles) sProcesses
    Scale typ n -> do
      let ps = map (scaleProcess typ n) sProcesses
      ps' <- mapM (updateProcess chan) ps
      let state' = state { sProcesses = ps' }
      processChan state' chan stateVar

-- | Compile itself.
compile :: Sentry -> IO Bool
compile state = do
  home <- getHomeDirectory
  let sentry = home </> ".sentryd"
      conf = sentry </> "conf"
      binPath = sExecutablePath state
      sourcePath = binPath <.> "hs"
      errorPath = binPath <.> "error"

  if takeDirectory binPath /= conf
    then do
      hPutStrLn stderr $ binPath ++ " is not under " ++ conf ++ "."
      return False
    else do
      status <- withFile errorPath WriteMode $ \h -> do
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
          hPutStrLn stderr $ "Problem encountered while compiling " ++ sourcePath ++ ":"
          hPutStrLn stderr content
          return False

-- | Save the application state then re-exec itself (calling `sentryd continue`).
reexecute :: Sentry -> IO a
reexecute state = do
  saveState state
  executeFile (sExecutablePath state) False ["continue"] Nothing

-- | Send a @SIGHUP@ signal to a running Sentry.
sendSIGHUP :: Sentry -> IO ()
sendSIGHUP state = do
  let binPath = sExecutablePath state
      pidPath = binPath <.> "pid"
      -- TODO all these xxxPath could be function on Sentry.
  -- TODO better error messages.
  content <- readFile pidPath
  signalProcess sigHUP $ fromIntegral (read content :: Int)

------------------------------------------------------------------------------
-- State
------------------------------------------------------------------------------

-- | Return the path where to save the application state.
getStatePath :: IO FilePath
getStatePath = do
  home <- getHomeDirectory
  return $ home </> ".sentryd" </> "sentry.state"

-- Inspired by the `executale-path` package, which implements
-- a similar function for different OS.
getExecutablePath :: IO FilePath
getExecutablePath = do
  pid <- fromIntegral <$> getProcessID :: IO Int
  readSymbolicLink $ "/proc/" ++ show pid ++ "/exe"

-- | Make sure the directory where the application state is saved exists.
ensureStateDirectory :: IO ()
ensureStateDirectory = do
  home <- getHomeDirectory
  let dir = home </> ".sentryd"
      conf = dir </> "conf"
  createDirectoryIfMissing False dir
  createDirectoryIfMissing False conf

-- | Create a initial application state from a list of process specifications.
initializeState :: [Entry] -> IO Sentry
initializeState specs = do
  path <- getExecutablePath
  t <- getTime
  return Sentry
    { sExecutablePath = path
    , sStartTime = t
    , sReexecTime = Nothing
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
          hPutStrLn stderr $ "Can't parse existing state." ++
            " Sentry continues with its current state." ++
            " The error was: " ++ err
          return Nothing
        Right a -> return $ Just a
    else do
      hPutStrLn stderr $ "The file `" ++ statePath ++
        "` doesn't exist. Sentry continues with its current state."
      return Nothing

-- | Given a list of "new" entries, either modify the monitored entry if it
-- must be updated (and return @Right@) or return @Left@ if it must be
-- deleted. @Left@ is used instead of @Nothing@ so the process handles can
-- be terminated if necessary.
continueProcess :: [Entry] -> MonitoredEntry ->
  Either MonitoredEntry MonitoredEntry
continueProcess entries m@MonitoredEntry{..} =
  case lookupProcess mEntry entries of
    Just p -> Right $ MonitoredEntry p mHandles
    Nothing -> Left m

-- | Add entries to a list of monitored entries if they are not already in
-- there.
addEntries :: [MonitoredEntry] -> [Entry] -> [MonitoredEntry]
addEntries = foldl' addEntry

-- | Add an entry to a list of monitored entries if it is not already in
-- there.
addEntry :: [MonitoredEntry] -> Entry -> [MonitoredEntry]
-- Order doesn't matter as it will be a Map anyway.
addEntry es e = if present then es else MonitoredEntry e [] : es
  where present = isJust $ lookupProcess e $ map mEntry es

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

------------------------------------------------------------------------------
-- Logging
------------------------------------------------------------------------------

colorize :: [MonitoredEntry] -> [MonitoredEntry]
colorize entries = zipWith f entries $ cycle $ map Just
  [Red, Blue, Green, Yellow, Magenta, Cyan, White]
  where f e c = e { mEntry = (mEntry e) { eColor = c } }

colorized :: Entry -> String -> String
colorized Entry{..} str =
  case eColor of
    Nothing -> pad str
    Just c ->
      setSGRCode [SetColor Foreground Dull c] ++  pad str ++ setSGRCode []

-- TODO non-hardcoded constant
pad :: String -> String
pad s = s ++ replicate (25 - length s) ' '

logP :: Entry -> Int -> String -> IO ()
logP p@Entry{..} i s = do
  ts <- getTimeString
  putStrLn $ colorized p (ts ++ " " ++ eType ++ "." ++ show i) ++ s

------------------------------------------------------------------------------
-- Signals
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

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

-- | Copy two handles to stdout. It is better if the handles are line-buffered.
pipeToStdout :: Entry -> Int -> Handle -> Handle -> IO ()
pipeToStdout p i h1 h2 = do
  eof1 <- hIsEOF h1
  eof2 <- hIsEOF h2
  ready1 <- if eof1 then return False else hReady h1
  ready2 <- if eof2 then return False else hReady h2
  when ready1 $ do
    l <- hGetLine h1
    logP p i l
  when ready2 $ do
    l <- hGetLine h2
    logP p i l
  when (not eof1 || not eof2) $
    pipeToStdout p i h1 h2
