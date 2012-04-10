-- |
-- Module      : Sentry
-- Copyright   : (c) 2012 Vo Minh Thu,
--
-- License     : BSD-style
-- Maintainer  : thu@hypered.be
-- Stability   : experimental
-- Portability : GHC
--
-- This module re-exports everything one needs to write a Sentry
-- configuration file. A configuration file is actually a (simple) Haskell
-- program. As an example, consider the following code:
--
-- > import Sentry
-- >
-- > main :: IO ()
-- > main = sentry
-- >   [ entry "dummy" "sleep" ["4"] 1000 2
-- >   ]
--
-- When compiled (some features don't work correctly under @ghci@) and run, the
-- above program will maintain two processes, both running @sleep@. Whenever a
-- process exits, Sentry will restart it. Detailed usage information can be
-- found at <https://github.com/noteed/sentry> and by running the program with
-- the @--help@ option.
--
-- See `sentry` and `entry` below for more details on those functions.
module Sentry
  ( sentry
  , entry
  ) where

import Sentry.Command (sentry)
import Sentry.Types (entry)
