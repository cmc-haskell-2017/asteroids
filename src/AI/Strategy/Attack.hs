module AI.Strategy.Attack where

import Models
import Config
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import AI.Strategy.Calculations
import AI.Strategy.Config

-- | Получение цели для атаки
getAttackTarget :: [Spaceship] -> Spaceship -> Point
getAttackTarget [] _ = (800*screenUp, 800*screenUp)
getAttackTarget (sh:shs) ship 
  | (dist1 < dist2) && (spaceshipPosition sh /= pos) && (enemy) = spaceshipPosition sh
  | otherwise = getAttackTarget shs ship
  where
    pos   = spaceshipPosition ship
    dist1 = distant pos (spaceshipPosition sh)
    dist2 = distant (getAttackTarget shs ship) pos
    enemy = group sh /= (group ship)

-- | Эвристика стратегии Атака цели
attackTargetHeuristic :: Point -> Spaceship -> Float
attackTargetHeuristic p s
  | nearEnemy < 5*at + 100 = critAt
  | otherwise             = at/nearEnemy 
  where
    nearEnemy = distant p (spaceshipPosition s)

-- | Действие при стратегии Атака цели
attackAction :: Point -> Spaceship -> ShipAction
attackAction p ship = ShipAction { 
    shipID       = spaceshipID ship
  , rotateAction = rotateAttack p ship
  , engineAction = engineAttack p ship
  , fireAction   = fireAttack p ship
  }

 -- | Определение направления поворота при стратегии Атака цели
rotateAttack :: Point -> Spaceship -> Maybe RotateAction
rotateAttack p ship
  | (ang > 0.1 && ang <= pi) && angDir > 0 = Just ToLeft
  | (ang > 0.1 && ang < pi) && angDir < 0 = Just ToRight
  | otherwise         = Nothing
  where
    ang       = divangAttack p ship
    angDir    = angleDir (norm $ shipDir ship) (norm $ vector (spaceshipPosition ship) p)

-- | Определение ускорения при стратегии Атака цели
engineAttack :: Point -> Spaceship -> Maybe EngineAction
engineAttack p ship
  | ang < 0.1    = Just Forward
  | otherwise    = Nothing
  where
    ang   = divangAttack p ship

-- | Определение необходимости огня при стратегии Атака цели
fireAttack :: Point -> Spaceship -> Bool
fireAttack p ship
  | divang' < pi/16 = True
  | otherwise       = False
  where
    divang'   = divangAttack p ship

-- | Основной угол между направлением корабля и направлением на цель
divangAttack :: Point -> Spaceship -> Float
divangAttack p ship = angleVV dir (norm enemydir)
   where
    dir       = (unitVectorAtAngle ((90 + (spaceshipDirection ship)) * pi / 180))
    enemydir  = vector (spaceshipPosition ship) p
