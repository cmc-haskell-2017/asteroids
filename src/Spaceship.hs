module Spaceship where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Interface.Pure.Game
import Config
import Models


-- | Начальное состояние корабля
initSpaceship :: Spaceship
initSpaceship = Spaceship
  { spaceshipPosition   = (0, 0)
  , spaceshipVelocity   = (0, 0) 
  , spaceshipAccelerate = 0
  , spaceshipAngularV   = 0
  , spaceshipDirection  = 0 
  , spaceshipSize       = 40
  , isfire              = False
  , fireReload          = 0
  }

-- | Инициализация пули
initBullet :: Spaceship -> Bullet
initBullet ship = Bullet
    { bulletPosition  = spaceshipPosition ship
        + rotateV (spaceshipDirection ship * pi / 180) (0, 60)
    , bulletVelocity  = rotateV (spaceshipDirection ship * pi / 180) (0, 10)
    , bulletDirection = spaceshipDirection ship
    , bulletSize      = 15
}

-- | Отобразить корабль.
drawSpaceship :: Picture -> Spaceship -> Picture
drawSpaceship image spaceship'
  = translate x y (rotate (- spaceshipDirection spaceship') image)
  where
    (x, y) = spaceshipPosition spaceship'

-- | Отобразить пули.
drawBullets :: Picture -> [Bullet] -> Picture
drawBullets _     []       = blank
drawBullets image bullets' = foldMap (drawBullet image) bullets'

-- | Отобразить пулю.
drawBullet :: Picture -> Bullet -> Picture
drawBullet image bullet =
  translate x y (rotate (- bulletDirection bullet) image)
  where
    (x, y) = bulletPosition bullet

-- | Движение корабля
moveShip :: Float -> Spaceship -> Spaceship
moveShip a ship = ship { spaceshipAccelerate = a }

-- | Поворот корабля
turnShip :: Float -> Spaceship -> Spaceship
turnShip a ship = ship { spaceshipAngularV = a }

-- | Выстрел корабля
fireSpaceship :: Spaceship -> [Bullet] -> [Bullet]
fireSpaceship ship bs = initBullet ship : bs

-- | Обновить состояние пуль
updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets t bullets' = filter visible (map (updateBullet t) bullets')
  where
    visible bullet = abs x <= screenRight && abs y <= screenUp
     where
        (x, y)  = bulletPosition bullet

-- | Обновить состояние одной пули
updateBullet :: Float -> Bullet -> Bullet
updateBullet t bullet = bullet
  { bulletPosition = newPosition }
  where
    newPosition = (bulletPosition bullet) + mulSV t (bulletVelocity bullet)


-- | Обновить состояние корабля.
updateSpaceship :: Float -> Spaceship -> Spaceship
updateSpaceship t ship = ship
  { spaceshipPosition  = updateShipPosition t ship
  , spaceshipVelocity  = updateShipVelocity t ship
  , spaceshipDirection = newDir
  , fireReload         = newReload
  }
  where
    shipDir = spaceshipDirection ship + t * spaceshipAngularV ship
    newDir
      | shipDir >  180 = shipDir - 360
      | shipDir < -180 = shipDir + 360
      | otherwise      = shipDir
    newReload
      | fireReload ship == reloadTime = 0
      | otherwise = fireReload ship + t

-- | Обновление положения корабля
updateShipPosition :: Float -> Spaceship -> Point
updateShipPosition t ship
  = ( checkBoards (fst(spaceshipPosition ship)) (fst newPos) screenRight
    , checkBoards (snd(spaceshipPosition ship)) (snd newPos) screenUp )
  where 
    newPos = spaceshipPosition ship + mulSV t (spaceshipVelocity ship)

-- | Проверка выхода за границы
checkBoards :: Float -> Float -> Float -> Float
checkBoards x y z
    | x >= 0    = min y (  z)
    | otherwise = max y (- z)
    
-- | Обновление скорости корабля
updateShipVelocity :: Float -> Spaceship -> Vector
updateShipVelocity t ship = velocity + mulSV t acceleration
  where
    acceleration = mulSV (spaceshipAccelerate ship)
      (unitVectorAtAngle ((90 + spaceshipDirection ship) * pi / 180))
    velocity = damping * cross * spaceshipVelocity ship
      where
        cross
          | crossX && crossY = (-1, -1)
          | crossX           = (-1,  1)
          | crossY           = ( 1, -1)
          | otherwise        = ( 1,  1)
          where
            crossX = abs (fst(updateShipPosition t ship)) == screenRight
            crossY = abs (snd(updateShipPosition t ship)) == screenUp
