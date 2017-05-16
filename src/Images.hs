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
  Just b_oil      <- loadJuicyPNG "images/oil.png"
  Just b_low     <- loadJuicyPNG "images/low_speed.png"
  Just b_fast      <- loadJuicyPNG "images/high_speed.png"
  Just b_gun      <- loadJuicyPNG "images/gun.png"
  Just stat        <- loadJuicyPNG "images/stat.png"
  Just shield      <- loadJuicyPNG "images/shield.png"
  return Images
    { imageBullet     = scale 0.07 0.07 bullet
    , imageAsteroid   = scale  1.0  1.0 asteroid
    , imageBackground = scale  1.5  1.5 background'
    , imageSpaceship  = scale  0.2  0.2 spaceship'
    , imageB_oil     = scale  0.06  0.06 b_oil
    , imageB_low     = scale  0.10  0.10 b_low
    , imageB_high     = scale  0.10  0.10 b_fast
    , imageB_gun     = scale  0.08  0.08 b_gun
    , imageStat       = scale  1.0  1.0 stat
    , imageShield     = scale  0.3  0.3 shield
    }
