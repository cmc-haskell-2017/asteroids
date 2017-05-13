module Universe where

import System.Random
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Data.Vector
import Asteroids
import Bonuses
import Spaceship
import Items
import Config
import Models
import AI
import Fisics

-- | Инициализация игровой вселенной без игроков (только с ботами)
emptyUniverse :: StdGen -> Universe
emptyUniverse g = Universe
  { bullets        = []
  , asteroids      = take asteroidsNumber (initAsteroids g)
  , bonuses        = take bonusesNumber   (initBonuses g)
  , spaceships     = initSpaceships g 1 botsNumber
  , playerID       = 0
  , background     = initBackground
  , table          = Nothing
  , freshPositions = initShipPositions g
  , freshAsteroids = drop asteroidsNumber (initAsteroids g)
  , freshBonuses   = drop bonusesNumber   (initBonuses g)
  , score          = 0
  }

-- | Инициализация игровой вселенной
initUniverse :: StdGen -> Universe
initUniverse g = (emptyUniverse g)
  { spaceships = initSpaceship Player (0,0) 1 1 : initSpaceships g 2 botsNumber
  , playerID   = 1
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
updateUniverse dt u = handleBotsActions (bulletsFaceSpaceships (bulletsFaceAsteroids (bonusesFaceSpaceships u
      { bullets        = updateBullets t newBullets
      , asteroids      = updateAsteroids t newAsteroids
      , bonuses        = updateBonuses t newBonuses
      , spaceships     = updateSpaceships t (spaceships u)
      , background     = updateBackground t u
      , freshPositions = tail (freshPositions u)
      , freshAsteroids = tail (freshAsteroids u) 
      , freshBonuses   = tail (freshBonuses u)
      })))
      where
        t = 60 * dt
        newAsteroids
          | length (asteroids u) < asteroidsNumber
            = head (freshAsteroids u) : asteroids u
          | otherwise = asteroids u
        newBonuses 
          | length (bonuses u) < bonusesNumber
            = head (freshBonuses u) : bonuses u
          | otherwise = bonuses u
        newBullets = (fireSpaceships (spaceships u)) ++ (bullets u)

-- | Обработка искусственного интелекта
handleBotsActions :: Universe -> Universe
handleBotsActions u = handleShipsAction (map (botAction u) (spaceships u)) u

-- | Сбросить игру.
resetUniverse :: StdGen -> Universe -> Universe
resetUniverse g _ = initUniverse g
