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

-- | Отобразить фон статистики.
drawBack :: Picture -> Maybe Table -> Maybe Picture
drawBack _ Nothing = Nothing
drawBack image (Just tablee) = Just (translate x y image)
  where
    (x, y) = tablePosition tablee

-- | Инициализация статистики
initTable :: Table
initTable = Table { tablePosition = (0, 0) }

-- | Инициализация статистики
initTableBack :: Table
initTableBack = Table { tablePosition = (-10, 0) }

-- | Отобразить фон для статистики
drawTableBack :: Maybe Table -> Maybe Picture
drawTableBack Nothing = Nothing
drawTableBack (Just tableback')
  = Just (translate (-w) h (scale 10 10 (pictures
            [ translate 56 (-20.5) (scale 0.02 0.02 (color white (text name)))
            , translate 38 (-23.5) (scale 0.01 0.01 (color white (text headers)))
            , translate 38 (-24.5) (scale 0.01 0.01 (color white (text metr)))
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
drawTable (Just table') shiftt score
  = Just (translate (-w) h (scale 10 10 (pictures
            [ translate 38 (-shiftt) (scale 0.015 0.015 (color white (text name))) 
            , translate 52 (-shiftt) (scale 0.015 0.015 (color white (text name1)))
            , translate 61 (-shiftt) (scale 0.015 0.015 (color white (text name2)))
            , translate 70 (-shiftt) (scale 0.015 0.015 (color white (text name3)))
            , translate 78 (-shiftt) (scale 0.015 0.015 (color white (text name4)))
            , translate 88 (-shiftt) (scale 0.015 0.015 (color white (text name5))) 
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
drawTables (Just tablee) (scoree : scorees) shiftt 
    = drawTable (Just tablee) shiftt scoree : drawTables (Just tablee) scorees (shiftt + 3)

drawMaybe :: [Maybe Picture] -> [Picture]
drawMaybe [] = [blank]
drawMaybe (Nothing : pics) = drawMaybe pics
drawMaybe ((Just pic) : pics) = pic : drawMaybe pics
