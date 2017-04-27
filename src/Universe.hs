module Universe where

import System.Random
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Data.Vector
import Asteroids
import Spaceship
import Items
import Config
import Models
import AI
import Fisics

-- | Инициализация игровой вселенной
initUniverse :: StdGen -> Universe
initUniverse g  = Universe
  { bullets        = []
  , asteroids      = take asteroidsNumber (initAsteroids g)
  , spaceships     = setSpaceshipsMode (initSpaceships 1 spaceshipsNumber)
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
  , backgroundVelocity = - spaceshipVelocity (head(spaceships u))
  }
  where
    newPos = backgroundPosition (background u) + mulSV t (backgroundVelocity (background u))

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u = handleBotsActions (bulletsFaceSpaceships (bulletsFaceAsteroids u
      { bullets        = updateBullets t newBullets
      , asteroids      = updateAsteroids t newAsteroids
      , spaceships      = updateSpaceships t (bullets u) (asteroids u) (spaceships u)
      , background     = updateBackground t u
      , freshAsteroids = tail (freshAsteroids u)
      }))
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
handleBotsActions :: Universe -> Universe
handleBotsActions u = handleShipsAction (Actions $ map (botAction u) (spaceships u)) u

-- | Сбросить игру.
resetUniverse :: StdGen -> Universe -> Universe
resetUniverse g _ = initUniverse g




