module Asteroids where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Interface.Pure.Game
import Config
import Images()
import Models

-- | Бесконечный список астероидов
asteroidList :: [Point] -> [Float] -> [Vector] -> [Float] -> [Asteroid]
asteroidList (p : pos) (d : dir) (v : vel) (s : siz)
  = initAsteroid p d v s : asteroidList pos dir vel siz
asteroidList _ _ _ _ = []

-- | Инициализация астероида
initAsteroid :: Point -> Float -> Vector -> Float -> Asteroid
initAsteroid pos dir vel siz
  = Asteroid
    { asteroidPosition  = newPos
    , asteroidDirection = dir
    , asteroidVelocity  = rotateV (dir * pi / 180) vel
    , asteroidSize      = siz
    }
    where
      (x, y) = pos
      w      = screenRight
      h      = screenUp
      newX
        | x < 0 && x > - w = x - w
        | x > 0 && x <   w = x + w
        | otherwise        = x
      newY
        | y < 0 && y > - h = y - h
        | y > 0 && y <   h = y + h
        | otherwise        = y
      newPos = (newX, newY)

-- | Инициализация астероидов
initAsteroids :: StdGen -> [Asteroid]
initAsteroids g = asteroidList (vectors xPositions yPositions g)
                               (floats directions g)
                               (vectors velocities velocities g)
                               (floats sizes g)


-- | Отобразить список астероидов
drawAsteroids :: Picture -> [Asteroid] -> Picture
drawAsteroids image asteroids' = foldMap (drawAsteroid image) asteroids'

-- | Отобразить астероид
drawAsteroid :: Picture -> Asteroid -> Picture 
drawAsteroid image asteroid
  = translate x y (resize (rotate (- asteroidDirection asteroid) image))
  where
    size   = asteroidSize asteroid
    resize = scale size size
    (x, y) = asteroidPosition asteroid

-- | Обновить астероиды игровой вселенной.
updateAsteroids :: Float -> [Asteroid] -> [Asteroid]
updateAsteroids t asteroids'
  = filter visible (map (updateAsteroid t) asteroids')
  where
    visible asteroid = abs x <= 2 * screenRight + radius
      && abs y <= 2 * screenUp + radius
      where
      (x, y)  = asteroidPosition asteroid
      radius  = asteroidSize asteroid * 70

-- | Обновить астероид
updateAsteroid :: Float -> Asteroid -> Asteroid 
updateAsteroid t asteroid = asteroid
  { asteroidPosition = newPosition }
  where
    newPosition = asteroidPosition asteroid + mulSV t (asteroidVelocity asteroid)

