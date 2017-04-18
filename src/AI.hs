module AI where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Models
import Fisics

instance Monoid Strategy where
    mempty = Strategy {velocity = (0,0), fire = False,  power = 0}
    mappend f g = Strategy {
          velocity = (mulSV (power f) (velocity f)) + (mulSV (power g) (velocity g))
        , fire = (fire f) || (fire g)
        , power = max (power f) (power g) 
    }

analyseUniverse :: Universe -> Spaceship -> Spaceship
analyseUniverse u ship =  reaction (checkAsteroids near ship) ship 
  where
    near = nearAsteroids (spaceshipPosition ship) (asteroids u)

nearAsteroids :: Point -> [Asteroid] -> [Asteroid]
nearAsteroids pship = filter (checkDistant 300 pship)

checkDistant :: Float -> Point -> Asteroid -> Bool
checkDistant d pship a = (distant pship (asteroidPosition a)) < d   

checkAsteroids :: [Asteroid] -> Spaceship -> Strategy
checkAsteroids as ship = mconcat (map (getStrategy ship) as)

getStrategy :: Spaceship -> Asteroid -> Strategy
getStrategy ship as = Strategy {
    velocity = mulSV (-1) (astDir + vdist)
  , fire     = checkFire ship as
  , power    = 1/(deviation)
  }
  where
    vdist = vector (spaceshipPosition ship) (asteroidPosition as)
    deviation = distant (spaceshipPosition ship) (asteroidPosition as)
    astDir = (unitVectorAtAngle ((90 + asteroidDirection as) * pi / 180))

reaction :: Strategy -> Spaceship -> Spaceship
reaction strat ship = ship {
    spaceshipAngularV = power' * 300
  , spaceshipAccelerate = velocity' *(power strat) 
  , isfire            = (fire strat)
  }  
  where
    power' = if mainang > 0 then (power strat) else (-1)*(power strat)
    velocity' = if (abs mainang > pi/2)  then (-20) else 20
    mainang = (angleVV dir (velocity strat))
    dir = (unitVectorAtAngle ((90 + spaceshipDirection ship) * pi / 180))

checkFire :: Spaceship -> Asteroid -> Bool
checkFire ship as = abs (argV vdist - shipDir) < 15
    where
        vdist = vector (spaceshipPosition ship) (asteroidPosition as)
        shipDir = (90 + spaceshipDirection ship) * pi / 180