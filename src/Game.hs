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
import Fisics
import AI

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
  ([ drawBackground (imageBackground images) (background u) 
  , drawBullets    (imageBullet images)     (bullets u)
  , drawAsteroids  (imageAsteroid images)   (asteroids u)
  , drawTable      (imageTable images)      (table u)
  , drawScore      (score u)
  ] 
  ++ drawSpaceships  (imageSpaceship images)  (spaceships u))

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработчик событий игры.
handleUniverse :: StdGen -> Event -> Universe -> Universe
handleUniverse _ (EventKey (SpecialKey KeyUp) Down _ _)    u
  = u { spaceships = moveShip 0.1 (spaceships u) }
handleUniverse _ (EventKey (SpecialKey KeyDown) Down _ _)  u
  = u { spaceships = moveShip (-0.1) (spaceships u) }
handleUniverse _ (EventKey (SpecialKey KeyUp) Up _ _)      u
  = u { spaceships = moveShip 0 (spaceships u) }
handleUniverse _ (EventKey (SpecialKey KeyDown) Up _ _)    u
  = u { spaceships = moveShip 0 (spaceships u) }
handleUniverse _ (EventKey (SpecialKey KeyLeft) Down _ _)  u
  = u { spaceships = turnShip 5 (spaceships u) }
handleUniverse _ (EventKey (SpecialKey KeyRight) Down _ _) u
  = u { spaceships = turnShip (-5) (spaceships u) }
handleUniverse _ (EventKey (SpecialKey KeyLeft) Up _ _)    u
  = u { spaceships = turnShip 0 (spaceships u) }
handleUniverse _ (EventKey (SpecialKey KeyRight) Up _ _)   u
  = u { spaceships = turnShip 0 (spaceships u) }
handleUniverse _ (EventKey (SpecialKey KeySpace) Down _ _) u
  = u { spaceships = (head(spaceships u)){isfire = True} : (tail(spaceships u))}
handleUniverse _ (EventKey (SpecialKey KeySpace) Up _ _) u
  = u { spaceships = (head(spaceships u)){isfire = False} : (tail(spaceships u))}
handleUniverse g (EventKey (SpecialKey KeyEnter) Down _ _) u
  = resetUniverse g u
handleUniverse _ _ u = u

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u = botsActions (bulletsFaceAsteroids u
      { bullets        = updateBullets t newBullets
      , asteroids      = updateAsteroids t newAsteroids
      , spaceships      = updateSpaceships t (bullets u) (asteroids u) (spaceships u)
      , background     = updateBackground t u
      , freshAsteroids = tail (freshAsteroids u)
      })
      where
        t = 60 * dt
        newAsteroids
          | length (asteroids u) < asteroidsNumber
            = head (freshAsteroids u) : asteroids u
          | otherwise = asteroids u
        newBullets
          | or (map (\ship -> isfire ship && fireReload ship == reloadTime) (spaceships u))
            = (fireSpaceships (spaceships u)) ++ (bullets u)
          | otherwise = bullets u


-- | Обработка искусственного интелекта
botsActions :: Universe -> Universe
botsActions u = u { spaceships = map (botAction u) (spaceships u) }

-- | Действие бота
botAction :: Universe -> Spaceship -> Spaceship
botAction u ship
   | (spaceshipMode ship) == Bot = analyseUniverse u ship
   | otherwise = ship 