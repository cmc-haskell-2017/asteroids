module AI.Strategy.Calculations where

import Models
import Config
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game

-- | Направление корабля в виде единичного вектора
shipDir :: Spaceship -> Vector
shipDir ship    = unitVectorAtAngle $ (90 + (spaceshipDirection ship)) * pi / 180 

-- | Направление угла
angleDir :: Vector -> Vector -> Float
angleDir (x, y) (u, v) = signum (x * v - y * u)

-- | Нормализация вектора
norm :: Vector -> Vector
norm (x,y) 
  | scal /= 0 = (x/scal, y/scal)
  | otherwise = (0, 0)
  where
    scal = sqrt(x*x + y*y)

-- | Проверка, что астероид на экране видим
visible :: Asteroid -> Bool
visible as = abs x <= screenRight + radius
      && abs y <= screenUp + radius
    where
      (x, y)  = asteroidPosition as
      radius  = asteroidSize as * 70

-- | Определение между объектами
distant :: Point -> Point -> Float
distant (x1, y1) (x2, y2) = sqrt(dx^2 + dy^2)
  where
    dx = x1 - x2
    dy = y1 - y2

-- | Вектор из двух точек
vector :: Point -> Point -> Vector
vector (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)