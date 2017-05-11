module AI.Models where

import Graphics.Gloss.Interface.Pure.Game

-- | Тактика
data Tactic = Avoidance | AttackTarget Point | BonusTarget Point  deriving(Eq)

-- | Стратегия
data Strategy = Strategy
  { tactic    :: Tactic
  , strShipID :: Int
  , heuristic :: Float
  }

-- | Моноид для стратегий
instance Monoid Strategy where
   mempty = Strategy{ tactic = Avoidance, strShipID = 0, heuristic = 0}
   mappend f g = Strategy { 
     strShipID = (strShipID f)
   , tactic = if (heuristic f) > (heuristic g) then (tactic f) else (tactic g)
   , heuristic = max (heuristic f) (heuristic g)
   }