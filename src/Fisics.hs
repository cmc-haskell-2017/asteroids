module Fisics where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line()
import Models
import Asteroids

-- | Столкновение пуль с астероидами
bulletsFaceAsteroids :: Universe -> Universe
bulletsFaceAsteroids u = u
  { asteroids  = newA
  , bullets    = newB
  , scores     = newS
  , spaceships = newShips
  , bonuses    = newBo
  }
  where
    a    = asteroids u
    b    = bullets u
    s    = spaceships u
    bo   = bonuses u
    newA  
      | any (\ship -> flag ship == True) s = []   
      | otherwise = asterFaceBullets b a ++ filter (not . asteroidFaceBullets b) a
    newB = filter (not . bulletFaceAsteroids a) b
    newBo 
      | any (\ship -> flag ship == True) s = []
      | otherwise = bo
    newS = newScore1 (filter (bulletFaceAsteroids a) b) (scores u) 
    newShips = updateShip s 
      
updateShip :: [Spaceship] -> [Spaceship]
updateShip [] = []
updateShip ships     
  | any (\ship -> flag ship == True) ships = map (\ship -> ship { flag = False, shieldTime = 20 }) ships
  | otherwise = ships

newScore1 :: [Bullet] -> [Score] -> [Score]
newScore1 [] scores' = scores'
newScore1 bullets' scores' = map (newScore2 bullets') scores'

newScore2 :: [Bullet] -> Score -> Score
newScore2 [] score = score
newScore2 bullets' s  
  | bulletID (head bullets') == scoreID s = s { scoreAst = scoreAst s + 1} 
  | otherwise = newScore2 (tail bullets') s

asterFaceBullets :: [Bullet] -> [Asteroid] -> [Asteroid]
asterFaceBullets [] _ = []
asterFaceBullets _ [] = []
asterFaceBullets bulls asts 
  | asteroidFaceBullets bulls (head asts) 
        =   initAsteroidd (asteroidPosition (head asts)) (asteroidDirection (head asts) + 30) 
                       (asteroidVelocity (head asts) + (0.5, 0.5))  (0.5 * asteroidSize (head asts))
          : initAsteroidd (asteroidPosition (head asts)) (asteroidDirection (head asts) - 30) 
                       (asteroidVelocity (head asts) + (-0.5, 0.5)) (0.5 * asteroidSize (head asts)) 
          : asterFaceBullets bulls (tail asts)
  | otherwise = asterFaceBullets bulls (tail asts)  

initAsteroidd :: Point -> Float -> Vector -> Float -> Asteroid
initAsteroidd pos dir vel siz
  = Asteroid
    { asteroidPosition  = pos
    , asteroidDirection = dir
    , asteroidVelocity  = rotateV (dir * pi / 180) vel
    , asteroidSize      = siz
    , astBonusTime      = (False, 0)
    }  

-- | Столкновение бонусов с кораблями
bonusesFaceSpaceships :: Universe -> Universe
bonusesFaceSpaceships u = u
  { bonuses = newBo
  , spaceships = newS
  , scores = newScore
  , asteroids = newAster
  }
  where
    s     = spaceships u
    bo    = bonuses u
    a     = asteroids u
    newBo = filter (not . bonusFaceSpaceships s) bo
    newS 
      | spaceshipFaceBonuses s bo = activBonus s (filter (bonusFaceSpaceships s) bo)
      | otherwise = s
    newScore = newScoree1 (filter (checkColl bo) s) (scores u)
    newAster 
      | any (\ship -> fst (bonIndex ship) == 7) newS =  map (\asteroid -> asteroid { astBonusTime = (True, 20) }) a
      | otherwise = a

newScoree1 :: [Spaceship] -> [Score] -> [Score]
newScoree1 [] scores' = scores'
newScoree1 spaceships' scores' = map (newScoree2 spaceships') scores'

newScoree2 :: [Spaceship] -> Score -> Score
newScoree2 [] score = score
newScoree2 spaceships' s  
  | spaceshipID (head spaceships') == scoreID s = s { scoreBonus = scoreBonus s + 1} 
  | otherwise = newScoree2 (tail spaceships') s

checkColl :: [Bonus] -> Spaceship -> Bool
checkColl bonuses' ship = shipFaceBonuses bonuses' ship

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
  = (spaceshipFaceBullet sh b && shieldTime sh == 0) || (bulletFaceSpaceships shs b)

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
actBonus bonuses' ship 
  | shipFaceBonuses bonuses' ship && whichBonusIs bonuses' == 1 
      = ship { shipLife = limit }
  | shipFaceBonuses bonuses' ship && whichBonusIs bonuses' == 6
      = ship { flag = True } 
  | shipFaceBonuses bonuses' ship && whichBonusIs bonuses' >= 2 && whichBonusIs bonuses' /= 6
      = ship { shipLife = isAlive
             , bonIndex = (whichBonusIs bonuses', 20)
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
whichBonusIs bonuses' 
  | any (check 7) bonuses' = 7
  | any (check 6) bonuses' = 6
  | any (check 5) bonuses' = 5
  | any (check 4) bonuses' = 4
  | any (check 3) bonuses' = 3  
  | any (check 2) bonuses' = 2
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