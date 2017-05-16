module Spaceship where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Interface.Pure.Game
import Config
import Models
import Fisics

-- | Начальное состояние корабля
initSpaceship :: Mode -> Point -> PlayerID -> Int -> Spaceship
initSpaceship mode pos ident gr = Spaceship
  { spaceshipID         = ident
  , spaceshipMode       = mode
  , lastAction          = mempty
  , group               = gr
  , spaceshipPosition   = pos
  , spaceshipVelocity   = (0, 0) 
  , spaceshipAccelerate = 0
  , spaceshipAngularV   = 0
  , spaceshipDirection  = 0 
  , spaceshipSize       = 40
  , shipLife            = 100
  , isfire              = False
  , fireReload          = 0
  , bonIndex            = (0, 0)
  , shieldTime          = 20
  }

-- | Создание бесконечного списка позиций для кораблей
initShipPositions :: StdGen -> [Point]
initShipPositions = vectors xShipPositions yShipPositions

-- | Создание списка кораблей
initSpaceships :: StdGen -> Int -> PlayerID -> [Spaceship]
initSpaceships _ _ 0 = []
initSpaceships g ident num = [(initSpaceship Bot pos ident 2)]
  ++ (initSpaceships g'' (ident + 1) (num - 1))
  where
    (x, g')  = randomR xShipPositions g
    (y, g'') = randomR yShipPositions g'
    pos      = (x,y)

initScore :: Mode -> Int -> Score
initScore mode ident = Score
  { scoreMode  = mode  
  , scoreID    = ident
  , scoreAst   = 0
  , scoreShip  = 0
  , scoreDeath = 0
  , scoreBonus = 0
  }

initScores :: Int -> Int -> [Score]
initScores _ 0 = []
initScores ident num = [(initScore Bot ident)] ++ (initScores (ident + 1) (num - 1) )

-- | Инициализация пули
initBullet :: Spaceship -> Bullet
initBullet ship = Bullet
    { bulletID        = spaceshipID ship
    , bulletPosition  = spaceshipPosition ship
        + rotateV (spaceshipDirection ship * pi / 180) (0, 60)
    , bulletVelocity  = rotateV (spaceshipDirection ship * pi / 180) (0, 10)
    , bulletDirection = spaceshipDirection ship
    , bulletSize      = 15
    }

-- | Отрисовка списка кораблей
drawSpaceships :: Picture -> Picture -> [Spaceship] -> [Picture]
drawSpaceships image1 image2 spaceships' = map (drawSpaceship image1 image2) spaceships'

-- | Отобразить корабль.
drawSpaceship :: Picture -> Picture -> Spaceship -> Picture
drawSpaceship image1 image2 spaceship'
  | shieldTime spaceship' > 0 =
      translate x y (pictures 
        [(rotate (- spaceshipDirection spaceship') image1)
        , (rotate (-spaceshipDirection spaceship') image2)
        , translate (-30) (50)  (scale 0.15 0.15 (color red (text name1)))
        , translate (-30) (75)  (scale 0.15 0.15 (color blue (text name2)))
        , translate (-30) (100) (scale 0.15 0.15 (color white (text name3)))
        ]) 
  | otherwise = 
      translate x y (pictures 
        [(rotate (- spaceshipDirection spaceship') image1)
        , translate (-30) (50)  (scale 0.15 0.15 (color red (text name1)))
        , translate (-30) (75)  (scale 0.15 0.15 (color blue (text name2)))
        , translate (-30) (100) (scale 0.15 0.15 (color white (text name3)))
        ])
  where
    (x, y) = spaceshipPosition spaceship'
    name1 
      | spaceshipMode spaceship' == Bot = "Bot " ++ show (spaceshipID spaceship')
      | otherwise = "Player " ++ show (spaceshipID spaceship')
    name2 = "Fuel "   ++ show (shipLife spaceship')
    name3 = "Bonus " ++ show (fst (bonIndex spaceship')) ++ " : " ++ show (snd (bonIndex spaceship'))

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
moveShip _ [] = []
moveShip a (ship:ships) = ship { spaceshipAccelerate = a } : ships

-- | Поворот корабля
turnShip :: Float -> [Spaceship] -> [Spaceship]
turnShip _ [] = []
turnShip a (ship:ships) = ship { spaceshipAngularV = a } : ships

-- | Выстрел корабля
fireSpaceships :: [Spaceship] -> [Bullet]
fireSpaceships [] = []
fireSpaceships (ship:ships) 
  | fire' ship = (initBullet ship) : (fireSpaceships ships)
  | otherwise = (fireSpaceships ships)
  where
    fire' ship' = isfire ship' && fireReload ship' >= reloadTime

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
  , shipLife           = isAlive
  , fireReload         = newReload
  , bonIndex           = overORnot
  , shieldTime         = protection
  }
  where
    shipDir 
      | fst (bonIndex ship) == 2 = spaceshipDirection ship + 0.1 * t * spaceshipAngularV ship
      | fst (bonIndex ship) == 3 = spaceshipDirection ship + 1.5 * t * spaceshipAngularV ship
      | otherwise = spaceshipDirection ship + t * spaceshipAngularV ship
    newDir
      | shipDir >  360 = shipDir - 360
      | shipDir <    0 = shipDir + 360
      | otherwise      = shipDir
    newReload
      | fst (bonIndex ship) == 4 && isfire ship == False = reloadTime
      | fst (bonIndex ship) == 4 && fireReload ship >= reloadTime = 0
      | fst (bonIndex ship) == 4 = fireReload ship + 5
      | fst (bonIndex ship) /= 4 && fireReload ship >= reloadTime && isfire ship == False = reloadTime
      | fst (bonIndex ship) /= 4 && fireReload ship >= reloadTime = 0
      | otherwise = fireReload ship + 1
    isAlive
      | shipLife ship <= 0 = 0
      | spaceshipAccelerate ship /= 0 = shipLife ship - 0.1
      | otherwise = shipLife ship - 0.001
    overORnot
      | snd (bonIndex ship) <= 0 = (0, 0)
      | otherwise = (fst (bonIndex ship), snd (bonIndex ship) - 0.1)
    protection
      | shieldTime ship <= 0 = 0
      | otherwise = shieldTime ship - 0.1

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
updateShipVelocity t ship 
  | shipLife ship == 0 = 0
  | otherwise = velocity' + mulSV t acceleration
  where
    acceleration 
      | fst (bonIndex ship) == 2 = mulSV (0.1 * (spaceshipAccelerate ship))
                  (unitVectorAtAngle ((90 + spaceshipDirection ship) * pi / 180))
      | fst (bonIndex ship) == 3 = mulSV (5.0 * (spaceshipAccelerate ship))
                  (unitVectorAtAngle ((90 + spaceshipDirection ship) * pi / 180))
      | otherwise = mulSV (spaceshipAccelerate ship)
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
  , spaceships = newS
  , scores     = newScore
  }
  where
    s    = spaceships u
    b    = bullets u
    a    = asteroids u
    newB = filter (not . bulletFaceSpaceships s) b
    newS = map (checkSpaceshipsCollisions u) s
    newScore = newScoreee4 (filter (checkCol a b) s)
                           (newScoreee1 (filter (bulletFaceSpaceships s) b) (scores u))
                   
newScoreee1 :: [Bullet] -> [Score] -> [Score]
newScoreee1 [] scores = scores
newScoreee1 bullets scores = map (newScoreee2 bullets) scores

newScoreee2 :: [Bullet] -> Score -> Score
newScoreee2 [] score = score
newScoreee2 (b:bs) s  
  | bulletID b == scoreID s = s { scoreShip = scoreShip s + 1} 
  | otherwise = newScoreee2 bs s

newScoreee4 :: [Spaceship] -> [Score] -> [Score]
newScoreee4 [] scores = scores
newScoreee4 ships scores = map (newScoreee3 ships) scores  

newScoreee3 :: [Spaceship] -> Score -> Score
newScoreee3 [] score = score
newScoreee3 (ship:ships) s
  | spaceshipID ship == scoreID s = s { scoreDeath = scoreDeath s + 1} 
  | otherwise = newScoreee3 ships s

checkCol :: [Asteroid] -> [Bullet] -> Spaceship -> Bool
checkCol asteroids bullets ship
  = (spaceshipFaceAsteroids [ship] asteroids || spaceshipFaceBullets [ship] bullets)
    && shieldTime ship == 0

-- | Проверка столкновений корабля с объектами, которые могут его уничтожить
checkSpaceshipsCollisions :: Universe -> Spaceship -> Spaceship
checkSpaceshipsCollisions u ship
  | (spaceshipFaceAsteroids [ship] asteroids' 
    || (spaceshipFaceBullets [ship] bullets')) && (shieldTime ship == 0)
    = initSpaceship (spaceshipMode ship) pos ident (group ship)
  | otherwise = ship
  where
    pos        = freshPositions u !! ident
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
handleShipsAction act u = u { spaceships = map (doActions act) (spaceships u) }

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
