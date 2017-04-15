module Universe where

import System.Random
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Data.Vector
import Asteroids
import Spaceship
import Items
import Config
import Models
import Fisics

-- | Инициализация игровой вселенной
initUniverse :: StdGen -> Universe
initUniverse g  = Universe
  { bullets        = []
  , asteroids      = take asteroidsNumber (initAsteroids g)
  , spaceship      = initSpaceship
  , background     = initBackground
  , table          = Nothing
  , freshAsteroids = drop asteroidsNumber (initAsteroids g)
  , score          = 0
  }

-- | Обновить фон
updateBackground :: Float -> Universe -> Background
updateBackground t u = Background
  { backgroundPosition 
      = (checkBoards (fst (backgroundPosition (background u))) (fst newPos) screenRight
        , checkBoards (snd(backgroundPosition (background u))) (snd newPos) screenUp)
  , backgroundVelocity = - spaceshipVelocity (spaceship u)
  }
  where
    newPos = backgroundPosition (background u) + mulSV t (backgroundVelocity (background u))

-- | Сбросить игру.
resetUniverse :: StdGen -> Universe -> Universe
resetUniverse g _ = initUniverse g

-- =========================================
-- Обновление игровой вселенной
-- =========================================

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u 
  | isGameOver u = u { table = Just initTable } -- resetUniverse g u
  | otherwise = bulletsFaceAsteroids u
      { bullets        = updateBullets t newBullets
      , asteroids      = updateAsteroids t newAsteroids
      , spaceship      = updateSpaceship t (spaceship u)
      , background     = updateBackground t u
      , freshAsteroids = tail (freshAsteroids u)
      }
      where
        t = 60 * dt
        newAsteroids
          | length (asteroids u) < asteroidsNumber
            = head (freshAsteroids u) : asteroids u
          | otherwise = asteroids u
        newBullets
          | isfire (spaceship u) && fireReload (spaceship u) == reloadTime
            = fireSpaceship (spaceship u) (bullets u)
          | otherwise = bullets u

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = spaceshipFaceAsteroids (spaceship u) (asteroids u)
  || spaceshipFaceBullets (spaceship u) (bullets u)



