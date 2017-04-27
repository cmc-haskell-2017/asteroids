module Fisics where

import Graphics.Gloss.Interface.Pure.Game
import Models


-- | Столкновение пуль с астероидами
bulletsFaceAsteroids :: Universe -> Universe
bulletsFaceAsteroids u = u
  { asteroids = newA
  , bullets   = newB
  , score     = score u + length b - length newB
  }
  where
    a    = asteroids u
    b    = bullets u
    newA = filter (not . asteroidFaceBullets b) a
    newB = filter (not . bulletFaceAsteroids a) b

-- | Астероид сталкивается с пулями?
asteroidFaceBullets :: [Bullet] -> Asteroid -> Bool
asteroidFaceBullets [] _ = False
asteroidFaceBullets bs a = any (asteroidFaceBullet a) bs

-- | Астероид сталкивается с пулей?
asteroidFaceBullet :: Asteroid -> Bullet -> Bool
asteroidFaceBullet a b = collision aPos aRad bPos bRad
  where 
    aPos = asteroidPosition a
    aRad = asteroidSize a * 70
    bPos = bulletPosition b
    bRad = bulletSize b

-- | Пуля сталкивается с астероидами?
bulletFaceAsteroids :: [Asteroid] -> Bullet -> Bool
bulletFaceAsteroids [] _ = False
bulletFaceAsteroids as b = any (bulletFaceAsteroid b) as

-- | Пуля сталкивается с астероидом?
bulletFaceAsteroid :: Bullet -> Asteroid -> Bool
bulletFaceAsteroid b a = collision aPos aRad bPos bRad
  where 
    aPos = asteroidPosition a
    aRad = asteroidSize a * 70
    bPos = bulletPosition b
    bRad = bulletSize b

-- | Пуля сталкивается с астероидами?
bulletFaceSpaceships :: [Spaceship] -> Bullet -> Bool
bulletFaceSpaceships [] _ = False
bulletFaceSpaceships (sh:shs) b 
  = (spaceshipFaceBullet sh b) || (bulletFaceSpaceships shs b)

-- | Определение столкновения корабля с астероидами
spaceshipFaceAsteroids :: [Spaceship] -> [Asteroid] -> Bool
spaceshipFaceAsteroids [] _ = False
spaceshipFaceAsteroids (ship : ships) as 
    = (any (spaceshipFaceAsteroid ship) as) || spaceshipFaceAsteroids ships as

-- | Определение столкновения корабля с астероидом
spaceshipFaceAsteroid :: Spaceship -> Asteroid -> Bool
spaceshipFaceAsteroid ship a = collision shipPos shipRad aPos aRad
  where
    shipPos = spaceshipPosition ship
    shipRad = spaceshipSize ship
    aPos    = asteroidPosition a
    aRad    = asteroidSize a * 70

-- | Определение столкновения с пулями
spaceshipFaceBullets :: [Spaceship] -> [Bullet] -> Bool
spaceshipFaceBullets [] _ = False
spaceshipFaceBullets (ship : ships) bs 
    = (any (spaceshipFaceBullet ship) bs) || spaceshipFaceBullets ships bs

-- | Определение столкновения с пулей
spaceshipFaceBullet :: Spaceship -> Bullet -> Bool
spaceshipFaceBullet ship b = collision shipPos shipRad bPos bRad
  where
    shipPos = spaceshipPosition ship
    shipRad = spaceshipSize ship
    bPos    = bulletPosition b
    bRad    = bulletSize b

-- | Определение пересечения двух окружностей
collision :: Point -> Float -> Point -> Float -> Bool
collision p1 r1 p2 r2 = d <= (r1 + r2)
  where
    d  = distant p1 p2

-- | Определение между объектами
distant :: Point -> Point -> Float
distant (x1, y1) (x2, y2) = sqrt(dx^2 + dy^2)
  where
    dx = x1 - x2
    dy = y1 - y2

-- | Вектор из двух точек
vector :: Point -> Point -> Vector
vector (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)