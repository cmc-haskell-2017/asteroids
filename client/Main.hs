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
import Items
import Game
import Config
import Models

data GameState = GameState
  { gameUniverse    :: TVar Universe
  , isShowTable     :: Bool
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
  _ <- forkIO $ sendBinaryData gameConnection (newShipAction (playerID u) Nothing (Just Forward) False u)
  return g
handleGame (EventKey (SpecialKey KeyDown) Down _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (newShipAction (playerID u) Nothing (Just Back) False u)
  return g
handleGame (EventKey (SpecialKey KeyUp) Up _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (nullAct 1 (newShipAction (playerID u) Nothing Nothing False u))
  return g
handleGame (EventKey (SpecialKey KeyDown) Up _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (nullAct 1 (newShipAction (playerID u) Nothing Nothing False u))
  return g
handleGame (EventKey (SpecialKey KeyLeft) Down _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (newShipAction (playerID u) (Just ToLeft) Nothing False u)
  return g
handleGame (EventKey (SpecialKey KeyRight) Down _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (newShipAction (playerID u) (Just ToRight) Nothing False u)
  return g
handleGame (EventKey (SpecialKey KeyLeft) Up _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (nullAct 2 (newShipAction (playerID u) Nothing Nothing False u))
  return g
handleGame (EventKey (SpecialKey KeyRight) Up _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (nullAct 2 (newShipAction (playerID u) Nothing Nothing False u))
  return g
handleGame (EventKey (SpecialKey KeySpace) Down _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (newShipAction (playerID u) Nothing Nothing True u)
  return g
handleGame (EventKey (SpecialKey KeySpace) Up _ _) g@GameState{..} = do
  u <- readTVarIO gameUniverse
  _ <- forkIO $ sendBinaryData gameConnection (nullAct 3 (newShipAction (playerID u) Nothing Nothing False u))
  return g
handleGame (EventKey (SpecialKey KeyTab) Down _ _) g@GameState{..}
  = return g { isShowTable = True }
handleGame (EventKey (SpecialKey KeyTab) Up _ _) g@GameState{..} =
  return g { isShowTable = False }
handleGame _ g = return g

handleUpdates :: GameState -> IO ()
handleUpdates GameState{..} = forever $ do
  universe <- receiveData gameConnection
  atomically $ writeTVar gameUniverse universe

drawGame :: Images -> GameState -> IO Picture
drawGame images GameState{..} = drawUniverse images <$> readTVarIO gameUniverse

updateGame :: Float -> GameState -> IO GameState
updateGame _ g@GameState{..} = do
  atomically $ do
    modifyTVar gameUniverse showStat
  return g
  where
    showStat u
      | isShowTable = u
        { tableback = Just initTableBack
        , table     = Just initTableBack
        }
      | otherwise = u

runIO :: Images -> IO ()
runIO images = do
  g        <- newStdGen
  universe <- atomically $ newTVar (emptyUniverse g)
  putStrLn "Input IP-adress"
  ipAddr   <- getLine
  runClient ipAddr 8000 "/connect" $ \conn -> do
    let gs = GameState universe False conn
    _ <- forkIO (handleUpdates gs)
    playIO display bgColor fps gs (drawGame images) handleGame updateGame
  where
    winOffset = (150, 150)
    display   = InWindow "Asteroids" (screenWidth, screenHeight) winOffset
    bgColor   = black
    fps       = 60
