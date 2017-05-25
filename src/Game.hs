module Game where

import Control.Concurrent.STM
import System.Random
import System.Exit
import Graphics.Gloss.Interface.IO.Game
import Universe
import Models
import Spaceship
import Asteroids
import Bonuses
import Items
import Config

run :: Images -> IO ()
run images = do
  g <- newStdGen
  initWorld <- atomically $ newTVar (initUniverse g)
  playIO display bgColor fps initWorld drawWorld (handleWorld g) updateWorld
  where
    display = InWindow "Asteroids" (screenWidth, screenHeight) (150, 150)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

    drawWorld w = do
      u <- readTVarIO w
      return (drawUniverse images u)

    handleWorld _ (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
    handleWorld g e w = atomically $ do
      u <- readTVar w
      writeTVar w (handleUniverse g e u)
      return w

    updateWorld dt w = do
      atomically $ modifyTVar w (updateUniverse dt)
      return w

-- =========================================
-- Отрисовка игровой вселенной
-- =========================================

-- | Отобразить игровую вселенную.
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  ([ drawBackground (imageBackground images) (background u) 
  , drawBullets   (imageBullet images)     (bullets u)
  , drawAsteroids (imageAsteroid images)   (asteroids u)
  , drawBonuses   [blank, (imageB_oil images), (imageB_low images), (imageB_high images), (imageB_gun images), (imageB_def images)] (bonuses u)  
  ] 
  ++ drawSpaceships  (imageSpaceship images) (imageShield images) [blank,(imageB_low images), (imageB_high images), (imageB_gun images), (imageB_def images)] (spaceships u)
  ++ drawMaybe [drawBack (imageStat images) (tableback u)]
  ++ drawMaybe [drawTableBack  (tableback u)]
  ++ drawMaybe (drawTables (table u) (scores u) 27.5) )

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработчик событий игры.
handleUniverse :: StdGen -> Event -> Universe -> Universe
handleUniverse g e u = handlePlayerAction g e u

-- | Обработка нажатий игрока
handlePlayerAction :: StdGen -> Event -> Universe -> Universe
handlePlayerAction  _ (EventKey (SpecialKey KeyUp) Down _ _)    u
    = handleShipsAction [newShipAction (playerID u) Nothing (Just Forward) False u] u
handlePlayerAction _ (EventKey (SpecialKey KeyDown) Down _ _)  u
    = handleShipsAction [newShipAction (playerID u) Nothing (Just Back) False u] u
handlePlayerAction _ (EventKey (SpecialKey KeyUp) Up _ _)      u
    = handleShipsAction [nullAct 1 (newShipAction (playerID u) Nothing Nothing False u)] u
handlePlayerAction _ (EventKey (SpecialKey KeyDown) Up _ _)    u
    = handleShipsAction [nullAct 1 (newShipAction (playerID u) Nothing Nothing False u)] u
handlePlayerAction _ (EventKey (SpecialKey KeyLeft) Down _ _)  u
    = handleShipsAction [newShipAction (playerID u) (Just ToLeft) Nothing False u] u
handlePlayerAction _ (EventKey (SpecialKey KeyRight) Down _ _) u
    = handleShipsAction [newShipAction (playerID u) (Just ToRight) Nothing False u] u
handlePlayerAction _ (EventKey (SpecialKey KeyLeft) Up _ _)    u
    = handleShipsAction [nullAct 2 (newShipAction (playerID u) Nothing Nothing False u)] u
handlePlayerAction _ (EventKey (SpecialKey KeyRight) Up _ _)   u
    = handleShipsAction [nullAct 2 (newShipAction (playerID u) Nothing Nothing False u)] u
handlePlayerAction _ (EventKey (SpecialKey KeySpace) Down _ _) u
    = handleShipsAction [newShipAction (playerID u) Nothing Nothing True u] u
handlePlayerAction _ (EventKey (SpecialKey KeySpace) Up _ _) u
    = handleShipsAction [nullAct 3 (newShipAction (playerID u) Nothing Nothing False u)] u
handlePlayerAction g (EventKey (SpecialKey KeyEnter) Down _ _) u
    = resetUniverse g u
handlePlayerAction _ (EventKey (SpecialKey KeyTab) Down _ _) u
    = u { tableback = Just initTableBack
          , table = Just initTable }
handlePlayerAction _ (EventKey (SpecialKey KeyTab) Up _ _) u
    = u { tableback = Nothing
          , table = Nothing }
handlePlayerAction _ _ u = u

-- | Новое действие игрока после нажатия
newShipAction :: Int -> Maybe RotateAction -> Maybe EngineAction -> Bool -> Universe-> ShipAction
newShipAction i r e b u = mappend
                          (initShipAction i r e b) 
                          (lastAction (findShip i (spaceships u)))

-- | Удаление влияния действия после отпускания кнопки
nullAct :: Int -> ShipAction -> ShipAction
nullAct 1 act = act { engineAction = Nothing }
nullAct 2 act = act { rotateAction = Nothing }
nullAct 3 act = act { fireAction = False }
nullAct _ _   = mempty
