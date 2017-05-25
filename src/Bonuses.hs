module Bonuses where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Interface.Pure.Game
import Config
import Images()
import Models

-- | Бесконечный список бонусов
bonusList :: [Int] -> [Point] -> [Float] -> [Vector] -> [Bonus]
bonusList (n : num) (p : pos) (d : dir) (v : vel)
  = initBonus n p d v : bonusList num pos dir vel
bonusList _ _ _ _ = []

-- | Инициализация бонуса
initBonus :: Int -> Point -> Float -> Vector -> Bonus
initBonus num pos dir vel
  = Bonus
    { bonusNumber    = num
    , bonusPosition  = newPos
    , bonusDirection = dir
    , bonusVelocity  = rotateV (dir * pi / 180) vel
    , bonusSize      = 1
    }
    where
      (x, y) = pos
      w      = screenRight
      h      = screenUp
      newX
        | x < 0 && x > - w = x - w
        | x > 0 && x <   w = x + w
        | otherwise        = x
      newY
        | y < 0 && y > - h = y - h
        | y > 0 && y <   h = y + h
        | otherwise        = y
      newPos = (newX, newY)

-- | Инициализация бонусов
initBonuses :: StdGen -> [Bonus]
initBonuses g = bonusList (ints numbers g)
                          (vectors xPositions yPositions g)
                          (floats directions g)
                          (vectors velocities velocities g)

-- | Отобразить список бонусов
drawBonuses :: [Picture] -> [Bonus] -> Picture
drawBonuses pics bonuses' = foldMap (drawBonus pics) bonuses'

-- | Отобразить бонус
drawBonus :: [Picture] -> Bonus -> Picture 
drawBonus pics bonus = translate x y (resize (rotate (- bonusDirection bonus) num))
  where
    size   = bonusSize bonus
    resize = scale size size
    (x, y) = bonusPosition bonus
    num = pics !! ident
    ident = bonusNumber bonus

-- | Обновить бонусы игровой вселенной.
updateBonuses :: Float -> [Bonus] -> [Bonus]
updateBonuses t bonuses'
  = filter visible (map (updateBonus t) bonuses')
  where
    visible bonus = abs x <= 2 * screenRight + radius
      && abs y <= 2 * screenUp + radius
      where
      (x, y)  = bonusPosition bonus
      radius  = bonusSize bonus * 10

-- | Обновить астероид
updateBonus :: Float -> Bonus -> Bonus
updateBonus t bonus = bonus
  { bonusPosition = newPosition }
  where
    newPosition = bonusPosition bonus + mulSV t (bonusVelocity bonus)