{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Sentry.Http
-- Copyright   : (c) 2012 Vo Minh Thu,
--
-- License     : BSD-style
-- Maintainer  : thu@hypered.be
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides an HTTP interface to Sentry.
module Sentry.Http where

import Control.Concurrent.MVar

import Snap.Core (MonadSnap,
                  method, Method(GET), route,
                  writeLBS)
import Snap.Http.Server (httpServe)
import qualified Snap.Http.Server.Config as Conf

import Sentry.Types (Sentry)

serve :: MVar Sentry -> IO ()
serve stateVar = httpServe conf $ handler stateVar
  where conf = Conf.setPort 8001 $
               Conf.setHostname "127.0.0.1" $
               Conf.defaultConfig


handler :: MonadSnap m => MVar Sentry -> m ()
handler stateVar = do
  route
    [ ("processes", method GET (writeLBS "coucou"))
    ]
