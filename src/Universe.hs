module Universe where

import System.Random
import Asteroids
import Bonuses
import Spaceship
import Items
import Config
import Models
import AI
import Fisics

-- | Инициализация игровой вселенной без игроков
emptyUniverse :: StdGen -> Universe
emptyUniverse g = Universe
  { bullets        = []
  , asteroids      = []
  , bonuses        = []
  , spaceships     = []
  , playerID       = 0
  , background     = initBackground
  , tableback      = Nothing
  , table          = Nothing
  , freshPositions = initShipPositions g1
  , freshAsteroids = initAsteroids g2
  , freshBonuses   = initBonuses g3
  , scores         = []
  }
  where
    (g1,g') = split g
    (g2,g3) = split g'

-- | Инициализация игровой вселенной
initUniverse :: StdGen -> Universe
initUniverse g = (emptyUniverse g)
  { spaceships = initSpaceship Player (0,0) 1 1 : initSpaceships g 2 botsNumber
  , playerID   = 1
  , scores     = initScore Player 1 : initScores 2 botsNumber
  }

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u = handleBotsActions (bulletsFaceSpaceships (bulletsFaceAsteroids (bonusesFaceSpaceships u
      { bullets        = updateBullets t newBullets
      , asteroids      = updateAsteroids t newAsteroids
      , bonuses        = updateBonuses t newBonuses
      , spaceships     = updateSpaceships t (spaceships u)
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

-- | Поиск корабля с нужным ID
findShip :: Int -> [Spaceship] -> Spaceship
findShip i []           = initSpaceship Bot (0, 0) i 2
findShip i (ship:ships)
  | (spaceshipID ship) == i = ship
  | otherwise = findShip i ships
