module Spaceship where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Interface.Pure.Game
import Config
import Models
import Fisics

-- | Начальное состояние корабля
initSpaceship :: Mode -> Point -> PlayerID -> Spaceship
initSpaceship mode pos ident = Spaceship
  { spaceshipID         = ident
  , spaceshipMode       = mode
  , lastAction          = mempty
  , spaceshipPosition   = pos
  , spaceshipVelocity   = (0, 0) 
  , spaceshipAccelerate = 0
  , spaceshipAngularV   = 0
  , spaceshipDirection  = 0 
  , spaceshipSize       = 40
  , isfire              = False
  , fireReload          = 0
  }

-- | Создание бесконечного списка позиций для кораблей
initShipPositions :: StdGen -> [Point]
initShipPositions = vectors xShipPositions yShipPositions

-- | Создание списка кораблей
initSpaceships :: StdGen -> Int -> PlayerID -> [Spaceship]
initSpaceships _ _ 0 = []
initSpaceships g ident num = [(initSpaceship Bot pos ident)]
  ++ (initSpaceships g'' (ident + 1) (num - 1))
  where
    (x, g')  = randomR xShipPositions g
    (y, g'') = randomR yShipPositions g'
    pos      = (x,y)

-- | Отдать корабль под управление игрока
setPlayerMode :: Spaceship -> Spaceship
setPlayerMode ship = ship {spaceshipMode = Player}

-- | Установка кораблей, управляемых игроками
setSpaceshipsMode :: [Spaceship] -> [Spaceship]
setSpaceshipsMode ships = (map setPlayerMode (take playersNumber ships))
  ++ (drop playersNumber ships) 

-- | Инициализация пули
initBullet :: Spaceship -> Bullet
initBullet ship = Bullet
    { bulletPosition  = spaceshipPosition ship
        + rotateV (spaceshipDirection ship * pi / 180) (0, 60)
    , bulletVelocity  = rotateV (spaceshipDirection ship * pi / 180) (0, 10)
    , bulletDirection = spaceshipDirection ship
    , bulletSize      = 15
}

-- | Отрисовка списка кораблей
drawSpaceships :: Picture -> [Spaceship] -> [Picture]
drawSpaceships image spaceships' = map (drawSpaceship image) spaceships'

-- | Отобразить корабль.
drawSpaceship :: Picture -> Spaceship -> Picture
drawSpaceship image spaceship'
  = translate x y (pictures 
    [(rotate (- spaceshipDirection spaceship') image)
    , translate (-30) (50) (scale 0.15 0.15 (color red (text name)))
    ])
  where
    (x, y) = spaceshipPosition spaceship'
    name = "Player " ++ show (spaceshipID spaceship')

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
moveShip :: Float -> [Spaceship] -> [Spaceship]
moveShip a ships = (head ships) { spaceshipAccelerate = a } : (tail ships)

-- | Поворот корабля
turnShip :: Float -> [Spaceship] -> [Spaceship]
turnShip a ships = (head ships) { spaceshipAngularV = a } : (tail ships)

-- | Выстрел корабля
fireSpaceships :: [Spaceship] -> [Bullet]
fireSpaceships [] = []
fireSpaceships (ship:ships) 
  | fire' ship = (initBullet ship) : (fireSpaceships ships)
  | otherwise = (fireSpaceships ships)
  where
    fire' ship' = isfire ship' && fireReload ship' == reloadTime

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
      | shipDir >  360 = shipDir - 360
      | shipDir <    0 = shipDir + 360
      | otherwise      = shipDir
    newReload
      | fireReload ship == reloadTime && isfire ship == False = reloadTime
      | fireReload ship == reloadTime = 0
      | otherwise = fireReload ship + t

-- | Обновление состояния списка кораблей
updateSpaceships :: Float -> [Spaceship] -> [Spaceship]
updateSpaceships t ships 
  = map (updateSpaceship t) ships

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
updateShipVelocity t ship = velocity' + mulSV t acceleration
  where
    acceleration = mulSV (spaceshipAccelerate ship)
      (unitVectorAtAngle ((90 + spaceshipDirection ship) * pi / 180))
    velocity' = damping * cross * spaceshipVelocity ship
      where
        cross
          | crossX && crossY = (-1, -1)
          | crossX           = (-1,  1)
          | crossY           = ( 1, -1)
          | otherwise        = ( 1,  1)
          where
            crossX = abs (fst(updateShipPosition t ship)) == screenRight
            crossY = abs (snd(updateShipPosition t ship)) == screenUp

-- | Столкновение пуль с астероидами
bulletsFaceSpaceships :: Universe -> Universe
bulletsFaceSpaceships u = u {
    bullets    = newB
  , score      = newScore
  , spaceships = newS
  }
  where
    s    = spaceships u
    b    = bullets u
    newB = filter (not . bulletFaceSpaceships s) b
    newS = map (checkSpaceshipsCollisions u) s
    newScore 
       | spaceshipFaceAsteroids s (asteroids u) 
         || (spaceshipFaceBullets s b) = 0
       | otherwise = score u

checkSpaceshipsCollisions :: Universe -> Spaceship -> Spaceship
checkSpaceshipsCollisions u ship
  | spaceshipFaceAsteroids [ship] asteroids' 
    || (spaceshipFaceBullets [ship] bullets') 
    = initSpaceship (spaceshipMode ship) pos ident
  | otherwise = ship
  where
    pos        = head $ freshPositions u
    asteroids' = asteroids u
    bullets'   = bullets u
    ident      = spaceshipID ship

-- | Создание действия корабля
initShipAction :: Int -> Maybe RotateAction -> Maybe EngineAction -> Bool -> ShipAction
initShipAction i r e b = ShipAction {
                           shipID = i
                         , rotateAction = r
                         , engineAction = e
                         , fireAction = b
                         }

-- | Обработка действий кораблей
handleShipsAction :: [ShipAction] -> Universe -> Universe
handleShipsAction act u = u {spaceships = map (doActions act) (spaceships u)}

-- | Обработка действий корабля
doActions :: [ShipAction] -> Spaceship -> Spaceship
doActions [] ship = ship
doActions (act:acts) ship 
   | (shipID act) == (spaceshipID ship) = doAction act ship
   | otherwise = doActions acts ship

-- | Обработка действия корабля
doAction :: ShipAction -> Spaceship -> Spaceship
doAction act ship = ship {
    spaceshipAccelerate = acc
  , spaceshipAngularV = ang
  , isfire = (fireAction act)
  , lastAction = act
  }
  where
    ang
      | (rotateAction act) == Just ToLeft  = 3
      | (rotateAction act) == Just ToRight = -3
      | (rotateAction act) == Nothing      = 0
      | otherwise                          = 0
    acc
      | (engineAction act) == Just Forward = 0.1
      | (engineAction act) == Just Back    = -0.1
      | (engineAction act) == Nothing      = 0 
      | otherwise                          = 0
