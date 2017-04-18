module Universe where

import System.Random
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Data.Vector
import Asteroids
import Spaceship
import Items
import Config
import Models

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

-- | Сбросить игру.
resetUniverse :: StdGen -> Universe -> Universe
resetUniverse g _ = initUniverse g

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver _ = False -- spaceshipFaceAsteroids (spaceships u) (asteroids u)
  -- || spaceshipFaceBullets (spaceships u) (bullets u)



