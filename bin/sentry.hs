module Main where

import Sentry

main :: IO ()
main = sentry
  [ entry "short" "sleep" ["2"] 1000 1
  , entry "long" "sleep" ["10"] 1000 2
  , entry "verylong" "sleep" ["30"] 1000 1
  ]
