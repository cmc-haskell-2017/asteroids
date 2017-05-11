module AI.Strategy.Bonus where

import Models
import Config
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import AI.Strategy.Calculations

getBonusTarget :: [Bonus] -> Spaceship -> Point
getBonusTarget [] _ = (800*screenUp, 800*screenUp)
getBonusTarget (b:bs) ship 
  | (dist1 < dist2) && (visibleBonus b) = bonusPosition b
  | otherwise = getBonusTarget bs ship
  where
    pos   = spaceshipPosition ship
    dist1 = distant pos (bonusPosition b)
    dist2 = distant (getBonusTarget bs ship) pos

bonusTargetHeuristic :: Point -> Spaceship -> Float
bonusTargetHeuristic p s
  | shipLife s < 20 = 0.98
  | otherwise = 50/nearLife 
  where
    nearLife = distant p (spaceshipPosition s)

bonusAction :: Point -> Spaceship -> ShipAction
bonusAction p ship = ShipAction { 
    shipID       = spaceshipID ship
  , rotateAction = rotateBonus p ship
  , engineAction = engineBonus p ship
  , fireAction   = False
  }

 -- | Определение направления поворота при стратегии ухода
rotateBonus :: Point -> Spaceship -> Maybe RotateAction
rotateBonus p ship
  | (ang > 0.01 || ang > pi - 0.01) && angDir > 0 = Just ToLeft
  | (ang > 0.01 || ang > pi - 0.01) && angDir < 0 = Just ToRight
  | otherwise         = Nothing
  where
    ang    = divangBonus p ship
    angDir = angleDir (norm $ shipDir ship) (norm p)

-- | Определение ускорения при стратегии ухода
engineBonus :: Point -> Spaceship -> Maybe EngineAction
engineBonus p ship
  | ang < 0.2    = Just Forward
  | ang > pi-0.2 = Just Back
  | otherwise    = Nothing
  where
    ang    = divangBonus p ship

-- | Основной вектор-цель, с направлением которого должно совпасть направление корабля
divangBonus :: Point -> Spaceship -> Float
divangBonus p ship = angleVV (norm p) (norm $ shipDir ship)