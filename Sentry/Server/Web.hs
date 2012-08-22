{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Sentry.Server.Web
-- Copyright   : (c) 2012 Vo Minh Thu,
--
-- License     : BSD-style
-- Maintainer  : thu@hypered.be
-- Stability   : experimental
-- Portability : GHC
--
-- This module implements a web interface to Sentry.
module Sentry.Server.Web where

forkServer :: S.ByteString  -- ^ Host to listen on (e.g. \"localhost\")
           -> Int           -- ^ Port to listen on (e.g. 8000)
           -> IO Server
forkServer host port = do
    counters <- newIORef M.empty
    gauges <- newIORef M.empty
    labels <- newIORef M.empty
    tid <- forkIO $ httpServe conf (monitor counters gauges labels)
    return $! Server tid counters gauges labels
  where conf = Config.setVerbose False $
               Config.setErrorLog Config.ConfigNoLog $
               Config.setAccessLog Config.ConfigNoLog $
               Config.setPort port $
               Config.setHostname host $
               Config.defaultConfig

