module Main where

import Sentry.Command
import Sentry.Types

main :: IO ()
main = sentry
  [ Process "short" "sleep" ["2"] 1000 1
  , Process "long" "sleep" ["10"] 1000 1
  , Process "verylong" "sleep" ["30"] 1000 1
  ]
