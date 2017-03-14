module Asteroids where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

run :: IO ()
run = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) drawUniverse handleUniverse updateUniverse
  where
    display = InWindow "Flappy Lambda" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- =========================================
-- Модель игровой вселенной
-- =========================================
-- |Точка
type Point = (Double, Double)

-- |Вектор
type Vector = (Double, Double, Double)

-- | Игровая вселенная
data Universe = Universe
{ asteroids :: [Asteroid] -- ^ Астероиды
, spaceship :: Spaceship -- ^ Космический корабль
}

-- | Астероид
data Asteroid = Asteroid
{ asteroidPosition :: Point -- ^ Положение астероида
, asteroidVelocity :: Vector -- ^ Скорость астероида
, asteroidSize :: Float -- ^ Размер астероида (Радиус/Диаметр)
} deriving (Eq, Show)

-- | Космический корабль
data Spaceship = Spaceship
{ spaceshipPosition :: Point -- ^ Положение корабля
, spaceshipVelocity :: Vector -- ^ Скорость корабля
, spaceshipDirection :: Vector --  ^ Направление корабля
, spaceshipSize :: Float -- ^ Размер корабля
} deriving (Eq, Show)

-- | Инициализация игровой вселенной.
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { asteroids  = initAsteroids g
  , spaceship = initSpaceship
  }
  
-- | Начальное состояние корабля.
initSpaceship :: Spaceship
initSpaceship = Spaceship
  { -- ????
  }
  
  -- | Инициализировать один астероид.
initAsteroid :: Point -> Asteroid
initAsteroid a = -- ???

-- | Инициализировать случайный бесконечный
-- список астероидов для игровой вселенной.
initAsteroids :: StdGen -> [Asteroid]
initAsteroids a = map initAsteroid
  --(??? a)

  -- =========================================
-- Отрисовка игровой вселенной
-- =========================================

-- | Отобразить игровую вселенную.
drawUniverse :: Universe -> Picture
drawUniverse u = pictures
  [ drawAsteroids  (asteroids u)
  , drawSpaceship (spaceship u)
  ]
  
drawAsteroids :: -- ???

drawSpaceship :: -- ???
  

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработчик событий игры.
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) = fireSpaceship
handleUniverse _ = id

fireSpaceship :: -- ???

-- =========================================
-- Обновление игровой вселенной
-- =========================================

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | isGameOver u = resetUniverse u
  | otherwise = u
      { asteroids  = updateAsteroids  dt (asteroids  u)
      , spaceship = updateSpaceship dt (spaceship u)
      }
  where
   -- ???

-- | Обновить состояние корабля.
updateSpaceship :: Float -> Spaceship -> Spaceship
updateSpaceship dt spaceship = -- ???

-- | Обновить астероиды игровой вселенной.
updateAsteroids :: Float -> [Asteroid] -> [Asteroid]
updateAsteroids _ [] = []
updateAsteroids -- ???

-- | Сбросить игру.
resetUniverse :: Universe -> Universe
resetUniverse u = u
  { asteroids  = tail (asteroids u)
  , spaceship = initSpaceship
  }

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = spaceshipFaceAsteroid
  where
    spaceshipFaceAsteroid = -- ???

-- =========================================
-- Константы, параметры игры
-- =========================================

-- | Ширина экрана.
screenWidth :: Int
screenWidth = 800

-- | Высота экрана.
screenHeight :: Int
screenHeight = 450

-- | Положение правого края экрана.
screenRight :: Offset
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана.
screenLeft :: Offset
screenLeft = - fromIntegral screenWidth / 2
