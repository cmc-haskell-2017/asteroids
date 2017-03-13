module Asteroids where

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
  , spaceship = initSpaceShip
  }
