module Config where

import System.Random
import Graphics.Gloss.Data.Vector
import Models

-- =========================================
-- Константы, параметры игры
-- =========================================

-- | Бесконечный список векторов
vectors :: (Float, Float) -> (Float, Float) -> StdGen -> [Vector]
vectors ran1 ran2 g
  = zipWith (\ x y -> (x, y)) (randomRs ran1 g1) (randomRs ran2 g2)
  where
    (g1, g2) = split g

-- | Бесконечный список чисел
floats :: (Float, Float) -> StdGen -> [Float]
floats range g = randomRs range g

-- | Количество астероидов
asteroidsNumber :: Int
asteroidsNumber = 100

-- | Количество ботов
botsNumber :: Int
botsNumber = 2

-- | ID корабля игрока
playerID :: PlayerID
playerID = 1

-- | Интервал позиций по оси абсцисс для астероидов
xPositions :: (Float, Float)
xPositions = (- width, width)
  where
    width = fromIntegral screenWidth

-- | Интервал позиций по оси ординат для астероидов
yPositions :: (Float, Float)
yPositions = (- height, height)
  where
    height = fromIntegral screenHeight

-- | Интервал позиций по оси абсцисс для кораблей
xShipPositions :: (Float, Float)
xShipPositions = (- width, width)
  where
    width = fromIntegral screenWidth / 2

-- | Интервал позиций по оси ординат для кораблей
yShipPositions :: (Float, Float)
yShipPositions = (- height, height)
  where
    height = fromIntegral screenHeight / 2

-- | Интервал скоростей для астероидов
velocities :: (Float, Float)
velocities = (0.0, 2.0)

-- | Интервал направлений для астероидов
directions :: (Float, Float)
directions = (0.0, 360.0)

-- | Интервал размеров для астероидов
sizes :: (Float, Float)
sizes = (0.5, 1.0)

-- | Затухание скорости корабля
damping :: (Float, Float)
damping = (0.98, 0.98)

-- | Время перезарядки
reloadTime :: Float
reloadTime = 30.0

-- | Ширина экрана.
screenWidth :: Int
screenWidth =  1366

-- | Высота экрана.
screenHeight :: Int
screenHeight = 768

-- | Положение верхнего края экрана.
screenUp :: Float
screenUp = fromIntegral screenHeight / 2

-- | Положение нижнего края экрана.
screenDown :: Float
screenDown = - fromIntegral screenHeight / 2

-- | Положение правого края экрана.
screenRight :: Float
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана.
screenLeft :: Float
screenLeft = - fromIntegral screenWidth / 2
