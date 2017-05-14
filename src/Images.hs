module Images where

import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Models

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just bullet      <- loadJuicyPNG "images/bullet.png"
  Just asteroid    <- loadJuicyPNG "images/asteroid.png"
  Just background' <- loadJuicyPNG "images/background.png"
  Just spaceship'  <- loadJuicyPNG "images/spaceship.png"
  Just bonus1      <- loadJuicyPNG "images/oil.png"
  Just bonus2      <- loadJuicyPNG "images/low_speed.png"
  Just bonus3      <- loadJuicyPNG "images/high_speed.png"
  Just bonus4      <- loadJuicyPNG "images/gun.png"
  Just stat        <- loadJuicyPNG "images/stat.png"
  return Images
    { imageBullet     = scale 0.07 0.07 bullet
    , imageAsteroid   = scale  1.0  1.0 asteroid
    , imageBackground = scale  1.5  1.5 background'
    , imageSpaceship  = scale  0.2  0.2 spaceship'
    , imageBonus1     = scale  0.06  0.06 bonus1
    , imageBonus2     = scale  0.10  0.10 bonus2
    , imageBonus3     = scale  0.10  0.10 bonus3
    , imageBonus4     = scale  0.08  0.08 bonus4
    , imageStat       = scale  1.0  1.0 stat
    }
