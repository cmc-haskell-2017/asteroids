module Asteroids where

run :: IO ()
run = putStrLn "This project is not yet implemented"

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
