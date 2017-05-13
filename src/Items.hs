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

-- | Отобразить фон.
drawBackground :: Picture -> Background -> Picture
drawBackground image background' = translate x y image
  where
    (x, y) = backgroundPosition background'

-- | Инициализация статистики
initTable :: Table
initTable = Table { tablePosition = (0, 0) }

-- | Отобразить статистику 
drawTable :: Score -> Maybe Table -> Maybe Picture
drawTable _ Nothing = Nothing
drawTable s (Just table) 
  = Just (translate (-w) h (scale 10 10 (pictures
            [ color white (polygon [ (50, -15.5), (50, -55.5), (90, -55.5), (90, -15.5) ]) -- белая рамка
            , color green (polygon [ (51, -16.5), (51, -54.5), (89, -54.5), (89, -16.5) ]) -- чёрные внутренности
            , translate 70 (-35.5) (scale 0.03 0.03 (color red (text (show s)))) -- красный счёт
          ])))
            where
              w = screenRight
              h = screenUp

drawMaybe :: Maybe Picture -> Picture
drawMaybe Nothing = blank
drawMaybe (Just pic) = pic




