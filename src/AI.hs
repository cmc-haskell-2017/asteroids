module AI where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Models
import Fisics
import Config

-- | Проверка, что кораблём управляет бот
botAction :: Universe -> Spaceship -> ShipAction
botAction u ship
   | (spaceshipMode ship) == Bot = analyseUniverse u ship
   | otherwise = mempty 

-- | Действие бота
analyseUniverse :: Universe -> Spaceship -> ShipAction
analyseUniverse u ship = strategyToAction (getStrategy u ship) u ship

-- | Вычисление стратегии
getStrategy :: Universe -> Spaceship -> Strategy
getStrategy _ ship = Strategy { tactic = OutTarget, strShipID = (spaceshipID ship)}

-- | Стратегия в действие
strategyToAction :: Strategy -> Universe -> Spaceship -> ShipAction
strategyToAction str u ship
  | (tactic str) == OutTarget = ShipAction { 
    shipID       = (strShipID str)
  , rotateAction = rotateOutTarget u ship
  , engineAction = engineOutTarget u ship
  , fireAction   = fireOutTarget u ship
  }
  | otherwise = mempty

-- | Определение направления поворота при стратегии ухода
rotateOutTarget :: Universe -> Spaceship -> Maybe RotateAction
rotateOutTarget u ship
  | (ang > 0.1 || ang > pi - 0.1) && angDir > 0 = Just ToRight
  | (ang > 0.1 || ang > pi - 0.1) && angDir < 0 = Just ToLeft
  | otherwise         = Nothing
  where
    ang    = divang u ship
    angDir = angleDir (norm $ shipDir ship) (norm vns)
    vns    = (-1)*(velocityNearSystem ship (asteroids u)) 

-- | Определение ускорения при стратегии ухода
engineOutTarget :: Universe -> Spaceship -> Maybe EngineAction
engineOutTarget u ship
  | vns == (0, 0) = Nothing
  | ang < 0.5    = Just Forward
  | ang > pi-0.5 = Just Back
  | otherwise     = Nothing
  where
    ang    = divang u ship
    vns    = (-1)*(velocityNearSystem ship (asteroids u))

-- | Определение необходимости огня при стратегии ухода
fireOutTarget :: Universe -> Spaceship -> Bool
fireOutTarget u ship
  | length(filter (nearTargetAst ship) (asteroids u)) == 0 = False
  | otherwise = True

-- | Ближайший астероид-цель для обстрела
nearTargetAst :: Spaceship -> Asteroid -> Bool
nearTargetAst ship as
   | nearAsteroidDist ship (filter visible [as]) < 800 = (divang' < pi/32)
   | otherwise = False
   where
    divang'  = angleVV dir astdir
    dir     = (unitVectorAtAngle ((90 + (spaceshipDirection ship)) * pi / 180))
    astdir  = vector (spaceshipPosition ship) (asteroidPosition as)

-- | Основной вектор-цель, с направлением которого должно совпасть направление корабля
divang :: Universe -> Spaceship -> Float
divang u ship 
  | vns /= (0, 0) = angleVV (norm vns) (norm $ shipDir ship)
  | otherwise     = 0
  where
    vns         = (-1)*(velocityNearSystem ship (asteroids u))
    
-- | Направление корабля в виде единичного вектора
shipDir :: Spaceship -> Vector
shipDir ship    = unitVectorAtAngle $ (90 + (spaceshipDirection ship)) * pi / 180 

-- | Направление угла
angleDir :: Vector -> Vector -> Float
angleDir (x, y) (u, v) = signum (x * v - y * u)

-- | Вектор движения системы ближайших векторов
velocityNearSystem :: Spaceship -> [Asteroid] -> Vector
velocityNearSystem ship as = foldl (+) (0, 0) (map (vel ship) (fil as)) 
 + norm (boardProtect ship)
 where
   fil      = filter (nearAsteroids (spaceshipPosition ship))
   vel s a  
      | (asteroidSize a) == 0 = (0, 0)
      | otherwise     = norm $ vector (spaceshipPosition s) (asteroidPosition a)

-- | Вектор действия стен на корабль для ориентирования ботов на поле
boardProtect :: Spaceship -> Vector
boardProtect ship = (bx, by)
   where
    x  = fst (spaceshipPosition ship)
    y  = snd (spaceshipPosition ship)
    by
      | y < screenDown/3 = screenDown/(y - screenDown)
      | y > screenUp/3 = screenUp/(screenUp - y)
      | otherwise = 0
    bx
      | x < screenLeft/3 = screenLeft/(x - screenLeft)
      | x > screenRight/3 = screenRight/(screenRight - x)
      | otherwise = 0 

-- | Расстояние до ближайшего астероида
nearAsteroidDist :: Spaceship -> [Asteroid] -> Float
nearAsteroidDist _ []        = screenRight*2
nearAsteroidDist ship (a:as) = min dist (nearAsteroidDist ship as)
  where
    dist = (distant (spaceshipPosition ship) (asteroidPosition a)) - (asteroidSize a)

-- | Проверка, что астероид находится достаточно близко к кораблю и видим
nearAsteroids :: Point -> Asteroid -> Bool
nearAsteroids p a = ((distant p (asteroidPosition a) - (asteroidSize a)) < 300)
      && (visible a)

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