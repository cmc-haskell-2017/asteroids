{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)

import Graphics.Gloss.Interface.IO.Game
import Network.WebSockets
import System.Random
import System.Environment
import System.Exit (exitSuccess)

import Universe
import Images
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
  [ip, port] <- getArgs
  images     <- loadImages
  runIO ip (read port :: Int) images

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
drawGame images GameState{..} = do
  u <- readTVarIO gameUniverse
  return $ drawUniverse images (showStat u)
  where
    showStat u
      | isShowTable = u
      | otherwise   = u
        { tableback = Nothing
        , table     = Nothing
        }

updateGame :: Float -> GameState -> IO GameState
updateGame _ g = return g

runIO :: String -> Int -> Images -> IO ()
runIO ip port images = do
  g        <- newStdGen
  universe <- atomically $ newTVar (emptyUniverse g)
  runClient ip port "/connect" $ \conn -> do
    let gs = GameState universe False conn
    _ <- forkIO (handleUpdates gs)
    playIO display bgColor fps gs (drawGame images) handleGame updateGame
  where
    winOffset = (150, 150)
    display   = InWindow "Asteroids" (screenWidth, screenHeight) winOffset
    bgColor   = black
    fps       = 60
