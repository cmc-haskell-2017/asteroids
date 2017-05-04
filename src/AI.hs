module AI where

import Models
import AI.Models
import AI.Strategy

-- | Проверка, что кораблём управляет бот
botAction :: Universe -> Spaceship -> ShipAction
botAction u ship
   | (spaceshipMode ship) == Bot = analyseUniverse u ship
   | otherwise = mempty 

-- | Действие бота
analyseUniverse :: Universe -> Spaceship -> ShipAction
analyseUniverse u ship = strategyToAction (tactic (getStrategy u ship)) u ship

-- | Вычисление стратегии
getStrategy :: Universe -> Spaceship -> Strategy
getStrategy u ship = mconcat [
      mS Avoidance
    , mS (AttackTarget gT)
    ]
    where
        mS a = makeStrategy a u ship
        gT   = getAttackTarget (spaceships u) ship

makeStrategy :: Tactic -> Universe -> Spaceship -> Strategy
makeStrategy t u s = Strategy {
    tactic    = t
  , strShipID = (spaceshipID s)
  , heuristic = getHeuristic t u s
  }

getHeuristic :: Tactic -> Universe -> Spaceship -> Float
getHeuristic Avoidance u s    = avoidanceHeuristic u s
getHeuristic (AttackTarget p) _ s = attackTargetHeuristic p s

-- | Стратегия в действие
strategyToAction :: Tactic -> Universe -> Spaceship -> ShipAction
strategyToAction Avoidance u ship    = avoidanceAction u ship
strategyToAction (AttackTarget p) _ ship = attackAction p ship