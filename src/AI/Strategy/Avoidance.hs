module AI.Strategy.Avoidance where

import Models
import Config
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import AI.Strategy.Calculations
import AI.Strategy.Config

-- | Эвристика стратегии Уворот от астероида
avoidanceHeuristic :: Universe -> Spaceship -> Float
avoidanceHeuristic u s
  | nearAst < 300 + av = critAv
  | boards             = av
  | otherwise          = av/nearAst 
  where
    nearAst = nearAsteroidDist s (asteroids u)
    boards  = (abs (fst (spaceshipPosition s))) > screenRight/3
      || (abs (snd (spaceshipPosition s))) > screenUp/3

-- | Дествие при стратегии Уворот от астероида
avoidanceAction :: Universe ->  Spaceship -> ShipAction
avoidanceAction u ship = ShipAction { 
    shipID       = spaceshipID ship
  , rotateAction = rotateAvoidance u ship
  , engineAction = engineAvoidance u ship
  , fireAction   = fireAvoidance   u ship
  }

 -- | Определение направления поворота при стратегии Уворот от астероида
rotateAvoidance :: Universe -> Spaceship -> Maybe RotateAction
rotateAvoidance u ship
  | (ang > 0.1 && ang < pi - 0.1) && angDir > 0 = Just ToRight
  | (ang > 0.1 && ang < pi - 0.1) && angDir < 0 = Just ToLeft
  | otherwise         = Nothing
  where
    ang    = divangAvoidance u ship
    angDir = angleDir (norm $ shipDir ship) (norm vns)
    vns    = (-1)*(velocityNearSystem ship (asteroids u)) 

-- | Определение ускорения при стратегии Уворот от астероида
engineAvoidance :: Universe -> Spaceship -> Maybe EngineAction
engineAvoidance u ship
  | vns == (0, 0) = Nothing
  | ang < 0.5    = Just Forward
  | ang > pi-0.5 = Just Back
  | otherwise     = Nothing
  where
    ang    = divangAvoidance u ship
    vns    = (-1)*(velocityNearSystem ship (asteroids u))

-- | Определение необходимости огня при стратегии Уворот от астероида
fireAvoidance :: Universe -> Spaceship -> Bool
fireAvoidance u ship
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

-- | Основной угол, между направлением корабля и направлением, куда нужно уйти
divangAvoidance :: Universe -> Spaceship -> Float
divangAvoidance u ship 
  | vns /= (0, 0) = angleVV (norm vns) (norm $ shipDir ship)
  | otherwise     = 0
  where
    vns         = (-1)*(velocityNearSystem ship (asteroids u))

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
nearAsteroidDist _ []        = screenRight*10
nearAsteroidDist ship (a:as) = min dist (nearAsteroidDist ship as)
  where
    dist = (distant (spaceshipPosition ship) (asteroidPosition a)) - (asteroidSize a)

-- | Проверка, что астероид находится достаточно близко к кораблю и видим
nearAsteroids :: Point -> Asteroid -> Bool
nearAsteroids p a = ((distant p (asteroidPosition a) - (asteroidSize a)) < 300)
      && (visible a)
