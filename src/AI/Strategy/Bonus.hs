module AI.Strategy.Bonus where

import Data.List (minimumBy)
import Models
import Config
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import AI.Strategy.Calculations
import AI.Strategy.Config

-- | Определение бонуса, который нужно взять
getBonusTarget :: [Bonus] -> Spaceship -> Point
getBonusTarget [] _ = (800*screenUp, 800*screenUp)
getBonusTarget b ship = minimumBy f (map bonusPosition  b)
  where
    pos = spaceshipPosition ship
    f pos1 pos2 = compare (distant pos1 pos) (distant pos2 pos)

-- | Эвристика стратегии Взять бонус
bonusTargetHeuristic :: Point -> Spaceship -> Float
bonusTargetHeuristic p s
  | shipLife s < 20 = critBs
  | otherwise = bs/nearLife 
  where
    nearLife = distant p (spaceshipPosition s)

-- | Действие при стратегии Взять бонус
bonusAction :: Point -> Spaceship -> ShipAction
bonusAction p ship = ShipAction { 
    shipID       = spaceshipID ship
  , rotateAction = rotateBonus p ship
  , engineAction = engineBonus p ship
  , fireAction   = False
  }

 -- | Определение направления поворота при стратегии Взять бонус
rotateBonus :: Point -> Spaceship -> Maybe RotateAction
rotateBonus p ship
  | (ang > 0.01 || ang > pi - 0.01) && angDir > 0 = Just ToLeft
  | (ang > 0.01 || ang > pi - 0.01) && angDir < 0 = Just ToRight
  | otherwise         = Nothing
  where
    ang       = divangBonus p ship
    angDir    = angleDir (norm $ shipDir ship) (norm $ vector (spaceshipPosition ship) p)

-- | Определение ускорения при стратегии Взять бонус
engineBonus :: Point -> Spaceship -> Maybe EngineAction
engineBonus p ship
  | ang < 0.2    = Just Forward
  | ang > pi-0.2 = Just Back
  | otherwise    = Nothing
  where
    ang    = divangBonus p ship

-- | Основной угол между направлением корабля и направлением на бонус-цель
divangBonus :: Point -> Spaceship -> Float
divangBonus p ship = angleVV dir (norm enemydir)
   where
    dir       = (unitVectorAtAngle ((90 + (spaceshipDirection ship)) * pi / 180))
    enemydir  = vector (spaceshipPosition ship) p