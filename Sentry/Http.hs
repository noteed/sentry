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
-- This module provides an HTTP interface to Sentry. It is similar to
-- tibbe's ekg package (and contains some of its code).
--
-- Example client:
--   > curl -H "Accept: text/plain" http://127.0.0.1:8001/entries
module Sentry.Http where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Word (Word8)

{-
import Snap.Core (MonadSnap, Request, Snap, finishWith, getHeaders, getRequest,
                  getResponse, method, Method(GET), modifyResponse, pass, route,
                  rqParams, rqPathInfo, setContentType, setResponseStatus,
                  writeBS, writeLBS)
-}
import Snap.Core (MonadSnap, Request, getHeaders, getRequest,
                  method, Method(GET), pass, route,
                  writeBS)
import Snap.Http.Server (httpServe)
import qualified Snap.Http.Server.Config as Conf

import Sentry.Types (Sentry, mEntry, showEntry, sProcesses)

serve :: MVar Sentry -> IO ()
serve stateVar = httpServe conf $ handler stateVar
  where conf = Conf.setPort 8001 $
               Conf.setHostname "127.0.0.1" $
               Conf.defaultConfig

handler :: MonadSnap m => MVar Sentry -> m ()
handler stateVar = do
  state <- liftIO $ readMVar stateVar
  route
    [ ("entries", method GET (format "text/plain" $ getEntries state))
    ]

getEntries state = mapM_ (writeBS . S8.pack . (++ "\n") . showEntry . mEntry) $
  sProcesses state

-- From tibbe's ekg package.

-- | Parse the HTTP accept string to determine supported content types.
parseHttpAccept :: S.ByteString -> [S.ByteString]
parseHttpAccept = map fst
                . sortBy (rcompare `on` snd)
                . map grabQ
                . S.split 44 -- comma
  where
    rcompare :: Double -> Double -> Ordering
    rcompare = flip compare
    grabQ s =
        let (s', q) = breakDiscard 59 s -- semicolon
            (_, q') = breakDiscard 61 q -- equals sign
         in (trimWhite s', readQ $ trimWhite q')
    readQ s = case reads $ S8.unpack s of
                (x, _):_ -> x
                _ -> 1.0
    trimWhite = S.dropWhile (== 32) -- space

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

-- | The Accept header of the request.
acceptHeader :: Request -> Maybe S.ByteString
acceptHeader req = S.intercalate "," <$> getHeaders "Accept" req

-- | Runs a Snap monad action only if the request's Accept header
-- matches the given MIME type.
format :: MonadSnap m => S.ByteString -> m a -> m a
format fmt action = do
    req <- getRequest
    let acceptHdr = (head . parseHttpAccept) <$> acceptHeader req
    case acceptHdr of
        Just hdr | hdr == fmt -> action
        _ -> pass
