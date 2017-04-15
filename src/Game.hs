module Game where

import System.Random
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Interface.Pure.Game
import Universe
import Models
import Spaceship
import Asteroids
import Items
import Config

run :: Images -> IO ()
run images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) (handleUniverse g) updateUniverse
  where
    display = InWindow "Asteroids" (screenWidth, screenHeight) (150, 150)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- =========================================
-- Отрисовка игровой вселенной
-- =========================================

-- | Отобразить игровую вселенную.
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  [ drawBackground (imageBackground images) (background u)
  , drawSpaceship  (imageSpaceship images)  (spaceship u) 
  , drawBullets    (imageBullet images)     (bullets u)
  , drawAsteroids  (imageAsteroid images)   (asteroids u)
  , drawTable      (imageTable images)      (table u)
  , drawScore      (score u)
  ]

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработчик событий игры.
handleUniverse :: StdGen -> Event -> Universe -> Universe
handleUniverse _ (EventKey (SpecialKey KeyUp) Down _ _)    u
  = u { spaceship = moveShip 0.1 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyDown) Down _ _)  u
  = u { spaceship = moveShip (-0.1) (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyUp) Up _ _)      u
  = u { spaceship = moveShip 0 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyDown) Up _ _)    u
  = u { spaceship = moveShip 0 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyLeft) Down _ _)  u
  = u { spaceship = turnShip 5 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyRight) Down _ _) u
  = u { spaceship = turnShip (-5) (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyLeft) Up _ _)    u
  = u { spaceship = turnShip 0 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyRight) Up _ _)   u
  = u { spaceship = turnShip 0 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeySpace) Down _ _) u
  = u { spaceship = (spaceship u){isfire = True}}
handleUniverse _ (EventKey (SpecialKey KeySpace) Up _ _) u
  = u { spaceship = (spaceship u){isfire = False}}
handleUniverse g (EventKey (SpecialKey KeyEnter) Down _ _) u
  = resetUniverse g u
handleUniverse _ _ u = u
