-- | A sentryd configuration to exercise it.
module Main where

import System.Posix.Process (getProcessID)

import Sentry.Server
import Sentry.Server.Command

main :: IO ()
main = do
  pid <- getProcessID
  -- sentry
  runCmd $ Start
    [ entry "short" "sentry-sleep" ["2"] 1000 1 Nothing
    , entry "normal" "sentry-sleep" ["5"] 1000 1 Nothing
    , entry "long" "sentry-sleep" ["10"] 1000 2 Nothing
    -- TODO Add an entry with output to stdout/stderr.
    , entry "kill" "sentry-kill" (killArgs pid) 1000 1 Nothing
    ]

  where

  killArgs pid =
    ["kill", "-n", "2", "--seconds", "11", "--sig", "2", "--pid", show pid]
