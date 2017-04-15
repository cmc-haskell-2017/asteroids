module Items where

import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Interface.Pure.Game
import Config
import Images()
import Models

-- | Инициализация фона
initBackground :: Background
initBackground = Background
   { backgroundPosition = (0, 0)
   , backgroundVelocity = (0, 0)
   }

-- | Инициализация заставки
initTable :: Table
initTable = Table { tablePosition = (0, 0) }

-- | Нарисовать счёт в левом верхнем углу экрана.
drawScore :: Score -> Picture
drawScore s = translate (-w) h (scale 10 10 (pictures
  [ color white (polygon [ (0, 0), (0, -6), (15, -6), (15, 0) ])          -- белая рамка
  , color black (polygon [ (0, 0), (0, -5.9), (14.9, -5.9), (14.9, 0) ])    -- чёрные внутренности
  , translate 4 (-4.5) (scale 0.03 0.03 (color red (text (show s))))  -- красный счёт
  ]))
  where
    w = screenRight
    h = screenUp

-- | Отобразить фон.
drawBackground :: Picture -> Background -> Picture
drawBackground image background' = translate x y image
  where
    (x, y) = backgroundPosition background'

-- | Отобразить заставку 
drawTable :: Picture -> Maybe Table -> Picture
drawTable _     Nothing       = blank
drawTable image (Just table') = translate x y image
  where
    (x, y) = tablePosition table'




