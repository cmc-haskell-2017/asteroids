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

-- | Столкновение бонусов с кораблями
bonusesFaceSpaceships :: Universe -> Universe
bonusesFaceSpaceships u = u
  { bonuses = newBo
  , spaceships = newS
  }
  where
    s     = spaceships u
    bo    = bonuses u
    newBo = filter (not . bonusFaceSpaceships s) bo
    newS 
      | spaceshipFaceBonuses s bo = activBonus s (filter (bonusFaceSpaceships s) bo)
      | otherwise = s

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

activBonus :: [Spaceship] -> [Bonus] -> [Spaceship]
activBonus [] _ = []
activBonus ships bonuses' = map (actBonus bonuses') ships

actBonus :: [Bonus] -> Spaceship -> Spaceship
actBonus bonuses ship 
  | shipFaceBonuses bonuses ship && whichBonusIs bonuses == 1 
      = ship { shipLife = limit }
  | shipFaceBonuses bonuses ship && (whichBonusIs bonuses >= 2) 
      = ship { shipLife = isAlive
             , bonIndex = (whichBonusIs bonuses, 20)
             }
  | otherwise = ship { shipLife = isAlive } 
  where
    isAlive 
      | shipLife ship <= 0 = 0
      | spaceshipAccelerate ship /= 0 = shipLife ship - 0.1
      | otherwise = shipLife ship - 0.001
    limit
      | shipLife ship + 50 < 100 = shipLife ship + 50
      | otherwise = 100

whichBonusIs :: [Bonus] -> Int
whichBonusIs [] = 0
whichBonusIs bonuses 
  | any (check 4) bonuses = 4
  | any (check 2) bonuses = 2
  | any (check 3) bonuses = 3
  | otherwise = 1

check :: Int -> Bonus -> Bool
check num bonus = bonusNumber bonus == num

unitVectorAtAnglee :: Float -> Vector
unitVectorAtAnglee r = (cos r, sin r)

bonusFaceSpaceships :: [Spaceship] -> Bonus -> Bool
bonusFaceSpaceships [] _ = False
bonusFaceSpaceships ships b = any (spaceshipFaceBonuss b) ships 

shipFaceBonuses :: [Bonus] -> Spaceship -> Bool
shipFaceBonuses [] _ = False
shipFaceBonuses bs ship = any (spaceshipFaceBonus ship) bs

spaceshipFaceBonuses :: [Spaceship] -> [Bonus] -> Bool
spaceshipFaceBonuses [] _ = False
spaceshipFaceBonuses (ship : ships) bs =
  (any (spaceshipFaceBonus ship) bs) || spaceshipFaceBonuses ships bs

spaceshipFaceBonus :: Spaceship -> Bonus -> Bool
spaceshipFaceBonus ship b = collision shipPos shipRad bPos bRad
  where
    shipPos = spaceshipPosition ship
    shipRad = spaceshipSize ship
    bPos    = bonusPosition b
    bRad    = bonusSize b * 50

spaceshipFaceBonuss :: Bonus -> Spaceship -> Bool
spaceshipFaceBonuss b ship = collision shipPos shipRad bPos bRad
  where
    shipPos = spaceshipPosition ship
    shipRad = spaceshipSize ship
    bPos    = bonusPosition b
    bRad    = bonusSize b * 50    

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