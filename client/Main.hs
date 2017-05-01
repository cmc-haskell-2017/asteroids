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

handleGame :: Event -> GameState -> IO GameState
handleGame (EventKey (SpecialKey KeyEsc) Down _ _) g = const exitSuccess g
handleGame (EventKey (SpecialKey KeyUp) Down _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (Actions [newShipAction playerID Nothing (Just Forward) False u])
  return g
handleGame (EventKey (SpecialKey KeyDown) Down _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (Actions [newShipAction playerID Nothing (Just Back) False u])
  return g
handleGame (EventKey (SpecialKey KeyUp) Up _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (Actions [nullAct 1 (newShipAction playerID Nothing Nothing False u)])
  return g
handleGame (EventKey (SpecialKey KeyDown) Up _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (Actions [nullAct 1 (newShipAction playerID Nothing Nothing False u)])
  return g
handleGame (EventKey (SpecialKey KeyLeft) Down _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (Actions [newShipAction playerID (Just ToLeft) Nothing False u])
  return g
handleGame (EventKey (SpecialKey KeyRight) Down _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (Actions [newShipAction playerID (Just ToRight) Nothing False u])
  return g
handleGame (EventKey (SpecialKey KeyLeft) Up _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (Actions [nullAct 2 (newShipAction playerID Nothing Nothing False u)])
  return g
handleGame (EventKey (SpecialKey KeyRight) Up _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (Actions [nullAct 2 (newShipAction playerID Nothing Nothing False u)])
  return g
handleGame (EventKey (SpecialKey KeySpace) Down _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (Actions [newShipAction playerID Nothing Nothing True u])
  return g
handleGame (EventKey (SpecialKey KeySpace) Up _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (Actions [nullAct 3 (newShipAction playerID Nothing Nothing False u)])
  return g
handleGame _ g = return g

handleUpdates :: GameState -> IO ()
handleUpdates GameState{..} = forever $ do
  universe <- receiveData gameConnection
  putStrLn "update"
  atomically $ writeTVar gameUniverse universe

drawGame :: Images -> GameState -> IO Picture
drawGame images GameState{..} = drawUniverse images <$> readTVarIO gameUniverse

updateGame :: Float -> GameState -> IO GameState
updateGame _ gs = return gs

runIO :: Images -> IO ()
runIO images = do
  g        <- newStdGen
  universe <- atomically $ newTVar (initUniverse g)
  runClient "localhost" 8000 "/connect" $ \conn -> do
      let gs = GameState universe conn
      _ <- forkIO (handleUpdates gs)
      playIO display bgColor fps gs (drawGame images) handleGame updateGame
  where
    winOffset = (150, 150)
    display   = InWindow "Asteroids" (screenWidth, screenHeight) winOffset
    bgColor   = black
    fps       = 60