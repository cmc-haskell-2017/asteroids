module Asteroids where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

run :: Images -> IO ()
run images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) handleUniverse (updateUniverse g)
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
	, imageAsteroid   = scale  0.5  0.5 asteroid
    , imageBackground = scale  1.5  1.5 background
    , imageSpaceship  = scale  0.2  0.2 spaceship
    }

-- =========================================
-- Модель игровой вселенной
-- =========================================

-- | Изображения объектов.
data Images = Images
  { imageBullet     :: Picture -- ^ Ракета.
  , imageAsteroid   :: Picture -- ^ Aстероид.
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
  { asteroidPosition  :: Point  -- ^ Положение астероида
  , asteroidDirection :: Float  -- ^ Направление астероида
  , asteroidVelocity  :: Vector -- ^ Скорость астероида
  , asteroidSize      :: Float  -- ^ Размер астероида
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

-- | Бесконечный список позиций для астероидов
positions :: StdGen -> Float -> Float -> [Point]
positions g k1 k2
-- Стоит добавить проверку на то,
-- что астероид будет создаваться там же,
-- где находится корабль
  = zipWith (\ x y -> (x, y)) (randomRs (-k1, k1) g) (randomRs (-k2, k2) g)

-- | Бесконечный список скоростей для астероидов
vectors :: StdGen -> Float -> Float -> [Vector]
vectors g k1 k2
  = zipWith (\ x y -> (x, y)) (randomRs (k1, k2) g) (randomRs (k1, k2) g)

-- | Бесконечный список направлений для астероидов
directions :: StdGen -> Float -> Float -> [Float]
directions g k1 k2 = randomRs (k1, k2) g

-- | Бесконечный список размеров для астероидов
sizes :: StdGen -> Float -> Float -> [Float]
sizes g k1 k2 = randomRs (k1, k2) g

-- | Инициализация игровой вселенной
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { bullets    = []
  , asteroids  = initAsteroids 3
                               (positions  g (fromIntegral screenWidth) (fromIntegral screenHeight))
                               (directions g      0.0 360.0)
                               (vectors    g      0.6   1.5)
                               (sizes      g      0.6   2.0) 
  , spaceship  = initSpaceship
  , background = initBackground
  }
  
-- | Инициализация фона
initBackground :: Background
initBackground = Background
   { backgroundPosition = (0, 0)
   , backgroundVelocity = (0, 0)
   }
  
-- | Инициализировать один астероид.
initAsteroid :: Point -> Float -> Vector -> Float -> Asteroid
initAsteroid position direction velocity size = Asteroid 
  { asteroidPosition  = position
  , asteroidDirection = direction
  , asteroidVelocity  = rotateV (direction * pi / 180) velocity
  , asteroidSize      = size
  }

-- | Инициализировать случайный бесконечный
-- список астероидов для игровой вселенной.
initAsteroids :: Int -> [Point] -> [Float] -> [Vector] -> [Float] ->[Asteroid]
initAsteroids 0 _ _ _ _ = []
initAsteroids n (p : positions) (d : directions) (v : velocities) (s : sizes)
  = initAsteroid p d v s : initAsteroids (n-1) positions directions velocities sizes

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

-- | Инициализация пули
initBullet :: Universe -> Bullet
initBullet u = Bullet
    { bulletPosition  = spaceshipPosition (spaceship u)
	  + rotateV (spaceshipDirection (spaceship u) * pi / 180) (0, 60)
    , bulletVelocity  = rotateV 
	    ((spaceshipDirection (spaceship u)) * pi / 180) (0, 10)
    , bulletDirection = spaceshipDirection (spaceship u)
    , bulletSize      = 0.07
    }

  -- =========================================
-- Отрисовка игровой вселенной
-- =========================================

-- | Отобразить игровую вселенную.
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  [ drawBackground (imageBackground images) (background u)
  , drawSpaceship  (imageSpaceship images)  (spaceship u) 
  , drawBullets    (imageBullet images)     (bullets u)
  , drawAsteroids  (imageAsteroid images)   (asteroids u)
  ]
  
drawAsteroids :: Picture -> [Asteroid] -> Picture
drawAsteroids image asteroids = pictures (map (drawAsteroid image) asteroids)

drawAsteroid :: Picture -> Asteroid -> Picture 
drawAsteroid image asteroid
  = scale size size (translate x y (rotate (- asteroidDirection asteroid) image))
  where
    size   = asteroidSize asteroid
    (x ,y) = asteroidPosition asteroid

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
  { bullets  = initBullet u : bullets u
  }

-- =========================================
-- Обновление игровой вселенной
-- =========================================

-- | Обновить состояние игровой вселенной.
updateUniverse :: StdGen -> Float -> Universe -> Universe
updateUniverse g dt u 
  | isGameOver u = resetUniverse g u
  | otherwise = bulletsFaceAsteroids u
      { bullets    = updateBullets (bullets u)
      , asteroids  = updateAsteroids dt (spaceshipVelocity (spaceship u)) (asteroids u) 
      , spaceship  = updateSpaceship (spaceship u)
      , background = updateBackground u
      }

-- | Столкновение пули с астероидами
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
updateAsteroids :: Float -> Vector -> [Asteroid] -> [Asteroid]
updateAsteroids _ _ [] = []
updateAsteroids dt v asteroids =  filter visible (map (updateAsteroid v) asteroids)
  where
    visible asteroid = (abs x) <= (2*fromIntegral screenWidth)
      && (abs y) <= (2*fromIntegral screenHeight)
	where
	  (x, y)  = asteroidPosition asteroid

updateAsteroid :: Vector -> Asteroid -> Asteroid 
updateAsteroid v asteroid = asteroid
	{ asteroidPosition = updateAsteroidPosition asteroid
   ,asteroidVelocity = updateAsteroidVelocity asteroid - mulSV 0.01 v
  }

updateAsteroidPosition :: Asteroid -> Vector
updateAsteroidPosition asteroid = (checkBoards (fst(asteroidPosition asteroid)) (fst newPos) w, checkBoards (snd(asteroidPosition asteroid)) (snd newPos) h)
  where
    newPos = (asteroidPosition asteroid) + (asteroidVelocity asteroid)
    w = fromIntegral screenWidth
    h = fromIntegral screenHeight

updateAsteroidVelocity :: Asteroid -> Vector
updateAsteroidVelocity asteroid = (velocityX, velocityY)
  where
    velocityX = crossX * fst (asteroidVelocity asteroid)
      where
        crossX
          | crossRight || crossLeft = - 1
          | otherwise               =   1
          where
            crossRight = fst(updateAsteroidPosition asteroid) ==   fromIntegral screenWidth
            crossLeft  = fst(updateAsteroidPosition asteroid) == - fromIntegral screenWidth

    velocityY = crossY * snd (asteroidVelocity asteroid)
      where
        crossY
          | crossUp || crossDown    = - 1
          | otherwise               =   1
          where
            crossUp    = snd(updateAsteroidPosition asteroid) ==   fromIntegral screenHeight
            crossDown  = snd(updateAsteroidPosition asteroid) == - fromIntegral screenHeight
-- | Сбросить игру.
resetUniverse :: StdGen -> Universe -> Universe
resetUniverse g u = u
  { asteroids  = initAsteroids 30
                               (positions  g (fromIntegral screenWidth) (fromIntegral screenHeight))
                               (directions g      0.0 360.0)
                               (vectors    g      0.6   1.5)
                               (sizes      g      0.6   2.0)  
  , bullets    = []
  , spaceship  = initSpaceship
  }

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = spaceshipFaceAsteroids u || spaceshipFaceBullets u

-- | Определение столкновения корабля(кораблей) с астероидами
spaceshipFaceAsteroids :: Universe -> Bool -- ??? -- Паше
spaceshipFaceAsteroids u = sfa (spaceshipPosition (spaceship u)) (spaceshipSize (spaceship u)) (asteroids u)

sfa :: Point -> Float -> [Asteroid] -> Bool
sfa _ _ [] = False
sfa pos rad (a:as) = (collision pos rad (asteroidPosition a) (asteroidSize a)) 
						|| (sfa pos rad as)

collision :: Point -> Float -> Point -> Float -> Bool
collision (x1, y1) r1 (x2, y2) r2 = d <= (50 + 50)
	where d = sqrt((x1-x2)^2 + (y1-y2)^2)


-- Если будет мультиплеер с несколькими кораблями (а он, скорее всего, будет)
-- | Определение столкновения с пулей
spaceshipFaceBullets :: Universe -> Bool -- ??? -- Паше
spaceshipFaceBullets _ = False

-- =========================================
-- Константы, параметры игры
-- =========================================

-- | Ширина экрана.
screenWidth :: Int
screenWidth =  1366

-- | Высота экрана.
screenHeight :: Int
screenHeight = 768

-- | Положение правого края экрана.
screenRight :: Float
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана.
screenLeft :: Float
screenLeft = - fromIntegral screenWidth / 2

-- | Скорость движения астероида по вселенной (в пикселях в секунду).
speed :: Float
speed = 100

-- | Небходимое расстояние между игроком и новоявленным астероидом.
playerOffset :: Float 
playerOffset = 300.0

-- | Расстояние между астероидами.
defaultOffset :: Float
defaultOffset = 150.0
