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

-- | Инициализация статистики
initTableBack :: Table
initTableBack = Table { tablePosition = (0, 0) }

-- | Отобразить фон для статистики
drawTableBack :: Maybe Table -> Maybe Picture
drawTableBack Nothing = Nothing
drawTableBack (Just tableback)
  = Just (translate (-w) h (scale 10 10 (pictures
            [ color blue (polygon [ (25, -18.5), (25, -55.5), (90, -55.5), (90, -18.5) ]) -- белая рамка
            , color white (polygon [ (26, -19.5), (26, -54.5), (89, -54.5), (89, -19.5) ]) -- чёрные внутренности
            , translate 44 (-23.5) (scale 0.02 0.02 (color black (text name)))
            , translate 28 (-26.5) (scale 0.01 0.01 (color black (text headers)))
            , translate 28 (-28.5) (scale 0.01 0.01 (color black (text metr)))
          ])))
            where
              w = screenRight
              h = screenUp
              name    = " Game Statistics"
              headers = " Bot / Player | Number | Killed asteroids | Killed ships | Deaths | Collected bonuses "
              metr    = " -------------------------------------------------------- "

-- | Отобразить статистику 
drawTable :: Maybe Table -> Float -> Score -> Maybe Picture
drawTable Nothing _ _  = Nothing
drawTable (Just table) shift score
  = Just (translate (-w) h (scale 10 10 (pictures
            [ translate 28 (-shift) (scale 0.01 0.01 (color black (text name))) 
            , translate 42 (-shift) (scale 0.01 0.01 (color black (text name1)))
            , translate 51 (-shift) (scale 0.01 0.01 (color black (text name2)))
            , translate 61 (-shift) (scale 0.01 0.01 (color black (text name3)))
            , translate 69 (-shift) (scale 0.01 0.01 (color black (text name4)))
            , translate 79 (-shift) (scale 0.01 0.01 (color black (text name5))) 
            ])))
            where
              w = screenRight
              h = screenUp
              name 
                | scoreMode score == Player = "  Player  " 
                | otherwise = "  Bot  "
              name1 =  show (scoreID score) 
              name2 =  show (scoreAst score) 
              name3 =  show (scoreShip score)
              name4 =  show (scoreDeath score)
              name5 =  show (scoreBonus score)

-- | Отрисовка полной статистики
drawTables :: Maybe Table -> [Score] -> Float -> [Maybe Picture]
drawTables Nothing _ _ = [Nothing]
drawTables _ [] _ = []
drawTables (Just tablee) (score : scores) shift 
    = drawTable (Just tablee) shift score : drawTables (Just tablee) scores (shift + 3)

drawMaybe :: [Maybe Picture] -> [Picture]
drawMaybe [] = [blank]
drawMaybe (Nothing : pics) = drawMaybe pics
drawMaybe ((Just pic) : pics) = pic : drawMaybe pics
