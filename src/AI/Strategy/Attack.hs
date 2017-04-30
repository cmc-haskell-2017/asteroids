module AI.Strategy.Attack where

import Models
import Config
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import AI.Strategy.Calculations

getAttackTarget :: [Spaceship] -> Point -> Point
getAttackTarget [] _ = (800*screenUp, 800*screenUp)
getAttackTarget (sh:shs) pos 
  | (dist1 < dist2) && (spaceshipPosition sh /= pos) = spaceshipPosition sh
  | otherwise = getAttackTarget shs pos
  where
    dist1 = distant pos (spaceshipPosition sh)
    dist2 = distant (getAttackTarget shs pos) pos

attackTargetHeuristic :: Point -> Spaceship -> Float
attackTargetHeuristic p s
  | nearEnemy < 500 = 0.99
  | otherwise = 100/nearEnemy 
  where
    nearEnemy = distant p (spaceshipPosition s)

attackAction :: Point -> Spaceship -> ShipAction
attackAction p ship = ShipAction { 
    shipID       = spaceshipID ship
  , rotateAction = rotateAttack p ship
  , engineAction = engineAttack p ship
  , fireAction   = fireAttack p ship
  }

 -- | Определение направления поворота при стратегии ухода
rotateAttack :: Point -> Spaceship -> Maybe RotateAction
rotateAttack p ship
  | (ang > 0.01 || ang > pi - 0.01) && angDir > 0 = Just ToLeft
  | (ang > 0.01 || ang > pi - 0.01) && angDir < 0 = Just ToRight
  | otherwise         = Nothing
  where
    ang    = divangAttack p ship
    angDir = angleDir (norm $ shipDir ship) (norm p)

-- | Определение ускорения при стратегии ухода
engineAttack :: Point -> Spaceship -> Maybe EngineAction
engineAttack p ship
  | ang < 0.01    = Just Forward
  | ang > pi-0.01 = Just Back
  | otherwise    = Nothing
  where
    ang    = divangAttack p ship

-- | Определение необходимости огня при стратегии ухода
fireAttack :: Point -> Spaceship -> Bool
fireAttack p ship
  | divang' < pi/16 = True
  | otherwise       = False
  where
    divang'   = angleVV dir enemydir
    dir       = (unitVectorAtAngle ((90 + (spaceshipDirection ship)) * pi / 180))
    enemydir  = vector (spaceshipPosition ship) p

-- | Основной вектор-цель, с направлением которого должно совпасть направление корабля
divangAttack :: Point -> Spaceship -> Float
divangAttack p ship = angleVV (norm p) (norm $ shipDir ship)