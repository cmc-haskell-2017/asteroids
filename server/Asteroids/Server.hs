{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Asteroids.Server where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad (forever)
import Control.Monad.Random (newStdGen)
import Data.Map (Map)
import qualified Data.Map as Map

import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant

import Universe
import Models
import Game ()
import Spaceship

type Client = Connection

data Config = Config
  { configUniverse :: TVar Universe
  , configClients  :: TVar (Map PlayerID Client)
  , configIDs      :: TVar [PlayerID]
  }

mkDefaultConfig :: IO Config
mkDefaultConfig = do
  g   <- newStdGen
  cfg <- atomically $ Config
    <$> newTVar (initUniverse g)
    <*> newTVar Map.empty
    <*> newTVar [1..]
  return cfg

type AsteroidsAPI = "connect" :> Raw

server :: Config -> Server AsteroidsAPI
server config = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      conn  <- acceptRequest pending_conn
      ident <- addClient conn config
      putStrLn $ "Player " ++ show ident ++ " joined!"
      handleActions ident conn config

    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

addClient :: Client -> Config -> IO PlayerID
addClient client Config{..} = atomically $ do
  ident:ids <- readTVar configIDs
  writeTVar configIDs ids
  modifyTVar configClients (Map.insert ident client)
  modifyTVar configUniverse (spawnPlayer ident)
  return ident

spawnPlayer :: PlayerID -> Universe -> Universe
spawnPlayer ident u = u { spaceships = map addPlayer $ spaceships u }
  where
    addPlayer ship
      | spaceshipID ship == ident = initSpaceship Player pos ident
      | otherwise                 = ship
    pos = head $ freshPositions u

handleActions :: PlayerID -> Connection -> Config -> IO ()
handleActions ident conn Config{..} = forever $ do
  action <- receiveData conn
  atomically $ do
    modifyTVar configUniverse (handleShipsAction [setID action])
    where

      setID :: ShipAction -> ShipAction
      setID act = act { shipID = ident }

periodicUpdates :: Int -> Config -> IO ()
periodicUpdates ms cfg@Config{..} = forever $ do
  threadDelay ms
  universe <- atomically $ do
    universe <- (updateUniverse dt) <$> readTVar configUniverse
    writeTVar configUniverse universe
    return universe
  broadcastUpdate universe cfg
  where
    dt = fromIntegral ms / 1000000

broadcastUpdate :: Universe -> Config -> IO ()
broadcastUpdate universe Config{..} = do
  clients <- readTVarIO configClients
  mapM_ (forkIO . sendUpdate) (Map.toList clients)
  where
    sendUpdate (ident, conn) = sendBinaryData conn (f universe) `catch` handleClosedConnection ident
    f u = u
      { freshAsteroids = []
      , freshPositions = []
      }

    handleClosedConnection :: PlayerID -> ConnectionException -> IO ()
    handleClosedConnection ident _ = do
      putStrLn ("Player " ++ show ident ++ " disconected.")
      atomically $ do
        modifyTVar configClients (Map.delete ident)
        modifyTVar configUniverse id --(kickPlayer ident)