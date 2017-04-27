{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)

import Graphics.Gloss.Interface.IO.Game
import Network.WebSockets
import System.Random
import System.Exit (exitSuccess)

import Universe
import Images
import Game
import Config
import Models

data GameState = GameState
  { gameUniverse    :: TVar Universe
  , gameConnection  :: Connection
  }

main :: IO ()
main = do
   images <- loadImages
   runIO images

handleGame :: StdGen -> Event -> GameState -> IO GameState
handleGame _ _ = return

handleUpdates :: GameState -> IO ()
handleUpdates GameState{..} = forever $ do
  universe <- receiveData gameConnection
  atomically $ writeTVar gameUniverse universe

drawGame :: Images -> GameState -> IO Picture
drawGame images GameState{..} = drawUniverse images <$> readTVarIO gameUniverse

updateGame :: Float -> GameState -> IO GameState
updateGame _ = return

runIO :: Images -> IO ()
runIO images = do
  g        <- newStdGen
  universe <- atomically $ newTVar (initUniverse g)
  runClient "localhost" 8000 "/connect" $ \conn -> do
    let gs = GameState universe conn
    _ <- forkIO (handleUpdates gs)
    playIO display bgColor fps gs (drawGame images) (handleGame g) updateGame
  where
    winOffset = (150, 150)
    display   = InWindow "Asteroids" (screenWidth, screenHeight) winOffset
    bgColor   = black
    fps       = 60