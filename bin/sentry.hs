{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Configurator
  ( autoConfig, autoReload, display
  , subscribe, prefix, Worth(..))
import Data.Version (showVersion)
import Paths_sentry (version)
import System.Console.CmdArgs.Implicit

import Sentry.Process

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes
    [ cmdSpawn -- just to try
    , cmdRespawn -- the important stuff
    , monitor -- just to try
    ]
  &= summary versionString
  &= program "sentry"

versionString :: String
versionString =
  "Sentry " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

data Cmd =
    Spawn
  | Respawn
  | Monitor
  deriving (Data, Typeable)

cmdSpawn :: Cmd
cmdSpawn = Spawn
  &= help "Spawn a command once"
  &= explicit
  &= name "spawn"

cmdRespawn :: Cmd
cmdRespawn = Respawn
  &= help "Spawn a command forever"
  &= explicit
  &= name "respawn"

monitor :: Cmd
monitor = Monitor
  &= help "Display modifications to sentry.conf."
  &= explicit
  &= name "monitor"

processCmd :: Cmd -> IO ()
processCmd Spawn{..} = do
  spawn "ls" []

processCmd Respawn{..} = do
  keepSpawned $ Process "main" "sleep" ["2"] 1000

processCmd Monitor{..} = do
  putStrLn "Sentry will report any change to `sentry.conf`. Hit Ctrl-C to exit."
  (config, t) <- autoReload autoConfig [Required "sentry.conf"]
  display config
  subscribe config (prefix "processes") handler
  forever $ do
    threadDelay (5 * 1000 * 1000)
  where handler name mvalue = print name >> print mvalue
