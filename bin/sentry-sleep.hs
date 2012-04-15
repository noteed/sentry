-- | Sample program that sleep some amount of time and prints to stdout and
-- stderr.
module Main where

import Control.Concurrent (threadDelay)
import System.IO (hPutStrLn, hSetBuffering, stderr, stdout, BufferMode(..))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  threadDelay $ 5 * 1000 * 1000
  hPutStrLn stdout "Printed on stdout."
  hPutStrLn stderr "Printed on stderr."
  threadDelay $ 5 * 1000 * 1000
