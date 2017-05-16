module Main where

import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (run)
import System.Environment

import Asteroids.Server

main :: IO ()
main = do
  [port] <- getArgs
  config <- mkDefaultConfig
  _      <- forkIO $ periodicUpdates 10000 config
  run (read port :: Int) $ server config
