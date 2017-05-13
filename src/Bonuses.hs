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
drawBonuses :: Picture -> Picture -> Picture -> Picture -> [Bonus] -> Picture
drawBonuses image1 image2 image3 image4 bonuses' = foldMap (drawBonus image1 image2 image3 image4) bonuses'

-- | Отобразить бонус
drawBonus :: Picture -> Picture -> Picture -> Picture -> Bonus -> Picture 
drawBonus image1 image2 image3 image4 bonus
  | bonusNumber bonus == 1 = translate x y (resize (rotate (- bonusDirection bonus) image1))
  | bonusNumber bonus == 2 = translate x y (resize (rotate (- bonusDirection bonus) image2))
  | bonusNumber bonus == 3 = translate x y (resize (rotate (- bonusDirection bonus) image3))
  | otherwise = translate x y (resize (rotate (- bonusDirection bonus) image4))
  where
    size   = bonusSize bonus
    resize = scale size size
    (x, y) = bonusPosition bonus

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