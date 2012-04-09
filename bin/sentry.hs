module Main where

import Sentry.Command
import Sentry.Types

main :: IO ()
main = sentry
  [ process "short" "sleep" ["2"] 1000 1
  , process "long" "sleep" ["10"] 1000 2
  , process "verylong" "sleep" ["30"] 1000 1
  ]
