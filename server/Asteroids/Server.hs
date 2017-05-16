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
import Items
import Spaceship

-- | Соединение с клиентом
type Client = Connection

-- | Конфигурация
data Config = Config
  { configUniverse :: TVar Universe              -- ^ Игровая вселенная
  , configClients  :: TVar (Map PlayerID Client) -- ^ Список клиентов
  , configIDs      :: TVar [PlayerID]            -- ^ Список свободных ID игроков/ботов
  }

-- | Создание начальной конфигурации
mkDefaultConfig :: IO Config
mkDefaultConfig = do
  g   <- newStdGen
  cfg <- atomically $ Config
    <$> newTVar (showTable (emptyUniverse g))
    <*> newTVar Map.empty
    <*> newTVar [1,3..]
  return cfg
  where
    showTable u = u
      { tableback = Just initTableBack
      , table     = Just initTable
      }

type AsteroidsAPI = "connect" :> Raw

-- | Сервер
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

-- | Добавление клиента в конфигурацию
addClient :: Client -> Config -> IO PlayerID
addClient client Config{..} = atomically $ do
  ident:ids <- readTVar configIDs
  writeTVar configIDs ids
  modifyTVar configClients (Map.insert ident client)
  modifyTVar configUniverse (spawnPlayer ident)
  return ident

-- | Создание игрока во вселенной
spawnPlayer :: PlayerID -> Universe -> Universe
spawnPlayer ident u = u
  { spaceships = addPlayer (spaceships u)
  , scores     = initScore Player ident : initScore Bot (ident + 1) : scores u
  }
  where
    addPlayer ships = initSpaceship Player pos1 ident 1 : initSpaceship Bot pos2 (ident + 1) 2 : ships
    pos1            = head $ freshPositions u
    pos2            = head $ tail $ freshPositions u

-- | Обработка действий клиентов
handleActions :: PlayerID -> Connection -> Config -> IO ()
handleActions ident conn Config{..} = forever $ do
  action <- receiveData conn
  atomically $ do
    modifyTVar configUniverse (handleShipsAction [setID action])
    where

      -- | Установить ID корабля
      setID :: ShipAction -> ShipAction
      setID act = act { shipID = ident }

-- | Обновление вселенной на сервере
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

-- | Отправка изменений на клиенты
broadcastUpdate :: Universe -> Config -> IO ()
broadcastUpdate universe Config{..} = do
  clients <- readTVarIO configClients
  mapM_ (forkIO . sendUpdate) (Map.toList clients)
  where
    sendUpdate (ident, conn) = sendBinaryData conn (f ident universe) `catch` handleClosedConnection ident
    f ident u = u
      { playerID       = ident
      , freshAsteroids = []
      , freshBonuses   = []
      , freshPositions = []
      }

    -- | Обработка отключения клиента
    handleClosedConnection :: PlayerID -> ConnectionException -> IO ()
    handleClosedConnection ident _ = do
      putStrLn ("Player " ++ show ident ++ " disconected.")
      atomically $ do
        modifyTVar configClients (Map.delete ident)
        modifyTVar configUniverse (kickPlayer ident)

-- | Удаление игрока с сервера
kickPlayer :: PlayerID -> Universe -> Universe
kickPlayer ident u = u
  { spaceships = filter isConnected $ spaceships u
  , scores     = filter isInTable $ scores u
  }
  where
    isConnected ship = ident /= spaceshipID ship && ident + 1 /= spaceshipID ship
    isInTable score  = ident /= scoreID score && ident + 1 /= scoreID score