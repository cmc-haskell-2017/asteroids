module Main where

import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (run)

import Asteroids.Server

main :: IO ()
main = do
  config <- mkDefaultConfig
  _      <- forkIO $ periodicUpdates 10000 config
  run 8000 $ server config
