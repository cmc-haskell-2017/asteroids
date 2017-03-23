module Asteroids where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

run :: Images -> IO ()
run images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) handleUniverse updateUniverse
  where
    display = InWindow "Asteroids" (screenWidth, screenHeight) (150, 150)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just bullet     <- loadJuicyPNG "images/bullet.png"
  Just asteroid   <- loadJuicyPNG "images/asteroid.png"
  Just background <- loadJuicyPNG "images/background.png"
  Just spaceship  <- loadJuicyPNG "images/spaceship.png"
  return Images
    { imageBullet     = scale 0.07 0.07 bullet
	, imageAsteroid   = scale  0.1  0.1 asteroid
    , imageBackground = scale  2.5  2.5 background
    , imageSpaceship  = scale  0.2  0.2 spaceship
    }

-- =========================================
-- Модель игровой вселенной
-- =========================================

-- | Изображения объектов.
data Images = Images
  { imageBullet     :: Picture -- ^ Ракета.
  , imageAsteroid   :: Picture -- ^ Изображение астероида.
  , imageBackground :: Picture -- ^ Фон.
  , imageSpaceship  :: Picture -- ^ Корабль.
  }

-- | Игровая вселенная
data Universe = Universe
  { asteroids  :: [Asteroid] -- ^ Астероиды
  , spaceship  :: Spaceship  -- ^ Космический корабль
  , background :: Background -- ^ Фон
  , bullets    :: [Bullet]   -- ^ Пули
  }

-- | Фон
data Background = Background
  { backgroundPosition :: Point  -- ^ Положение фона
  , backgroundVelocity :: Vector -- ^ Вектор скорости фона
  } deriving (Eq, Show)

-- | Астероид
data Asteroid = Asteroid
  { asteroidPosition :: Point  -- ^ Положение астероида
  , asteroidVelocity :: Vector -- ^ Скорость астероида
  , asteroidSize     :: Float  -- ^ Размер астероида
  } deriving (Eq, Show)

-- | Космический корабль
data Spaceship = Spaceship
  { spaceshipPosition   :: Point  -- ^ Положение корабля
  , spaceshipVelocity   :: Vector -- ^ Скорость корабля
  , spaceshipAccelerate :: Float  -- ^ Ускорение
  , spaceshipDirection  :: Float  -- ^ Направление корабля
  , spaceshipAngularV   :: Float  -- ^ Угловая скорость
  , spaceshipSize       :: Float  -- ^ Размер корабля
  } deriving (Eq, Show)

-- Пуля
data Bullet = Bullet
  { bulletPosition  :: Point  -- ^ Положение пули
  , bulletVelocity  :: Vector -- ^ Скорость пули
  , bulletDirection :: Float  -- ^ Направление пули
  , bulletSize      :: Float  -- ^ Размер пули
  } deriving (Eq, Show)

-- | Инициализация игровой вселенной
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { bullets    = []
  , asteroids  = initAsteroids g
  , spaceship  = initSpaceship
  , background = initBackground
  }
  
-- | Начальное состояние корабля
initSpaceship :: Spaceship
initSpaceship = Spaceship
  { spaceshipPosition   = (0, 0)
  , spaceshipVelocity   = (0, 0) 
  , spaceshipAccelerate = 0
  , spaceshipAngularV   = 0
  , spaceshipDirection  = 0 
  , spaceshipSize       = 1
  }

-- | Инициализация фона
initBackground :: Background
initBackground = Background
   { backgroundPosition = (0, 0)
   , backgroundVelocity = (0, 0)
   }

-- | Инициализация пули
initBullet :: Universe -> Bullet
initBullet u
  = Bullet
    { bulletPosition  = spaceshipPosition (spaceship u)
	  + rotateV (spaceshipDirection (spaceship u) * pi / 180) (0, 60)
    , bulletVelocity  = rotateV 
	    ((spaceshipDirection (spaceship u)) * pi / 180) (0, 10)
    , bulletDirection = spaceshipDirection (spaceship u)
    , bulletSize      = 0.07
    }
  
-- | Инициализировать один астероид.
-- initAsteroid :: Point -> Asteroid

-- | Инициализировать случайный бесконечный
-- список астероидов для игровой вселенной.
initAsteroids :: StdGen -> [Asteroid]
initAsteroids x = []

  -- =========================================
-- Отрисовка игровой вселенной
-- =========================================

-- | Отобразить игровую вселенную.
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  [ drawBackground (imageBackground images) (background u)
  , drawSpaceship  (imageSpaceship images)  (spaceship u) 
  , drawBullets    (imageBullet images)     (bullets u)
  --, drawAsteroids  (imageAsteroid images)   (asteroids u)
  ]
  
--drawAsteroids :: Picture -> [Asteroid] -> Picture -- ??? Тимуру

--drawAsteroid :: Pictire -> Asteroid -> Picture -- ??? Тимуру

-- | Отобразить фон.
drawBackground :: Picture -> Background -> Picture
drawBackground image background = translate x y image
  where
    (x, y) = backgroundPosition background

-- | Отобразить корабль.
drawSpaceship :: Picture -> Spaceship -> Picture
drawSpaceship image spaceship
  = translate x y (rotate (- spaceshipDirection spaceship) image)
  where
    (x, y) = spaceshipPosition spaceship

-- | Отобразить пули.
drawBullets :: Picture -> [Bullet] -> Picture
drawBullets image bullets = pictures (map (drawBullet image) bullets)

-- | Отобразить пулю.
drawBullet :: Picture -> Bullet -> Picture
drawBullet image bullet =
  translate x y (rotate (- bulletDirection bullet) image)
  where
    (x, y) = bulletPosition bullet

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработчик событий игры.
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeyUp) Down _ _)    = moveShip 0.1
handleUniverse (EventKey (SpecialKey KeyDown) Down _ _)  = moveShip (-0.1)
handleUniverse (EventKey (SpecialKey KeyUp) Up _ _)      = moveShip 0 
handleUniverse (EventKey (SpecialKey KeyDown) Up _ _)    = moveShip 0 
handleUniverse (EventKey (SpecialKey KeyLeft) Down _ _)  = turnShip (5) 
handleUniverse (EventKey (SpecialKey KeyRight) Down _ _) = turnShip (-5) 
handleUniverse (EventKey (SpecialKey KeyLeft) Up _ _)    = turnShip 0 
handleUniverse (EventKey (SpecialKey KeyRight) Up _ _)   = turnShip 0 
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) = fireSpaceship
handleUniverse _                                         = id

-- | Движение корабля
moveShip :: Float -> Universe -> Universe
moveShip a u = u 
  { spaceship = (spaceship u) { spaceshipAccelerate = a }
  }

-- | Поворот корабля
turnShip :: Float -> Universe -> Universe
turnShip a u = u 
  { spaceship = (spaceship u) { spaceshipAngularV = a }
  }

-- | Выстрел корабля
fireSpaceship :: Universe -> Universe
fireSpaceship u = u
  { bullets = initBullet u : bullets u
  }

-- =========================================
-- Обновление игровой вселенной
-- =========================================

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u  
  | isGameOver u = resetUniverse u
  | otherwise = bulletsFaceAsteroids u
      { bullets    = updateBullets (bullets u)
      , asteroids  = updateAsteroids (asteroids u)
      , spaceship  = updateSpaceship (spaceship u)
      , background = updateBackground u
      }

-- | Столкновение пулей с астероидами
bulletsFaceAsteroids :: Universe -> Universe
bulletsFaceAsteroids u = u

-- | Обновить состояние пуль
updateBullets :: [Bullet] -> [Bullet]
updateBullets bullets = filter visible (map updateBullet bullets)
  where
    visible bullet = (abs x) <= (fromIntegral screenWidth / 2)
	  && (abs y) <= (fromIntegral screenHeight / 2)
	where
	  (x, y)  = bulletPosition bullet

-- | Обновить состояние одной пули
updateBullet :: Bullet -> Bullet
updateBullet bullet = bullet
  { bulletPosition = (x, y) }
  where
    (x, y) = bulletPosition bullet + bulletVelocity bullet

-- | Обновить состояние корабля.
updateSpaceship :: Spaceship -> Spaceship
updateSpaceship spaceship = spaceship 
	{ spaceshipPosition  = updateShipPosition spaceship
	, spaceshipVelocity  = updateShipVelocity spaceship
	, spaceshipDirection = (if newDir > 180 then (-1) else (if newDir < -180 then (1) else 0))*360 + newDir 
	}
  where
    newDir = spaceshipDirection spaceship + spaceshipAngularV spaceship

-- | Обновление положения корабля
updateShipPosition :: Spaceship -> Vector
updateShipPosition ship = (checkBoards (fst(spaceshipPosition ship)) (fst newPos) w, checkBoards (snd(spaceshipPosition ship)) (snd newPos) h)
  where 
    newPos = (spaceshipPosition ship + spaceshipVelocity ship)
    w = fromIntegral screenWidth / 2
    h = fromIntegral screenHeight / 2

-- | Проверка выхода за границы
checkBoards :: Float -> Float -> Float -> Float
checkBoards x y z
    | x >= 0    = min y (  z)
    | otherwise = max y (- z)
    

-- | Обновление скорости корабля
updateShipVelocity :: Spaceship -> Vector
updateShipVelocity ship = (velocityX, velocityY) + acceleration
  where
    acceleration = mulSV (spaceshipAccelerate ship)
      (unitVectorAtAngle ((90 + spaceshipDirection ship) * pi / 180))
    velocityX = crossX * fst (spaceshipVelocity ship)
      where
        crossX
          | crossRight || crossLeft = - 0.99
          | otherwise               =   0.99
          where
            crossRight = fst(updateShipPosition ship) ==   fromIntegral screenWidth / 2
            crossLeft  = fst(updateShipPosition ship) == - fromIntegral screenWidth / 2

    velocityY = crossY * snd (spaceshipVelocity ship)
      where
        crossY
          | crossUp || crossDown    = - 0.99
          | otherwise               =   0.99
          where
            crossUp    = snd(updateShipPosition ship) ==   fromIntegral screenHeight / 2
            crossDown  = snd(updateShipPosition ship) == - fromIntegral screenHeight / 2

-- | Обновить фон
updateBackground :: Universe -> Background
updateBackground u = Background
  { backgroundPosition 
    = (checkBoards (fst(backgroundPosition (background u))) (fst newPos) w, checkBoards (snd(backgroundPosition (background u))) (snd newPos) h)
  ,backgroundVelocity = (- spaceshipVelocity (spaceship u))
  }
  where 
    newPos = (backgroundPosition (background u)) + (backgroundVelocity (background u))
    w = fromIntegral screenWidth / 2
    h = fromIntegral screenHeight / 2

-- | Обновить астероиды игровой вселенной.
updateAsteroids :: [Asteroid] -> [Asteroid]
updateAsteroids asteroids = asteroids -- ??? Тимур

updateAsteroid :: Asteroid -> Asteroid -- ??? Тимур
updateAsteroid asteroid = asteroid

-- | Сбросить игру.
resetUniverse :: Universe -> Universe
resetUniverse u = u
  { asteroids = tail (asteroids u)
  , bullets   = []
  , spaceship = initSpaceship
  }

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = spaceshipFaceAsteroids u || spaceshipFaceBullets u

-- | Определение столкновения корабля(кораблей) с астероидами
spaceshipFaceAsteroids :: Universe -> Bool -- ??? -- Паше
spaceshipFaceAsteroids _ = False

-- Если будет мультиплеер с несколькими кораблями (а он, скорее всего, будет)
-- | Определение столкновения с пулей
spaceshipFaceBullets :: Universe -> Bool -- ??? -- Паше
spaceshipFaceBullets _ = False

-- =========================================
-- Константы, параметры игры
-- =========================================

-- | Ширина экрана.
screenWidth :: Int
screenWidth = 1200

-- | Высота экрана.
screenHeight :: Int
screenHeight = 700

-- | Положение правого края экрана.
screenRight :: Float
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана.
screenLeft :: Float
screenLeft = - fromIntegral screenWidth / 2
