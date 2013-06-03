module Main where

import Sentry.Server

main :: IO ()
main = sentry
  [ entry "short" "sleep" ["2"] 1000 1 Nothing
  , entry "long" "sleep" ["10"] 1000 2 Nothing
  , entry "verylong" "sleep" ["30"] 1000 1 Nothing
  ]
