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
  , asteroidRadius    :: Float  -- ^ Радиус астероида
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
positions :: StdGen -> [Point]
positions g
  = zipWith (\ x y -> (x, y)) (randomRs (- width, width) g1) (randomRs (- height, height) g2)
  where
    (g1, g2) = split g
    width    = fromIntegral screenWidth / 2
    height   = fromIntegral screenHeight / 2

-- | Бесконечный список скоростей для астероидов
vectors :: StdGen -> [Vector]
vectors g
  = zipWith (\ x y -> (x, y)) (randomRs (0.5, 1.5) g1) (randomRs (0.5, 1.5) g2)
  where
    (g1, g2) = split g

-- | Бесконечный список направлений для астероидов
directions :: StdGen -> [Float]
directions g = randomRs (0.0, 360.0) g

-- | Бесконечный список размеров для астероидов
sizes :: StdGen -> [Float]
sizes g = randomRs (0.5, 2.0) g

-- | Инициализация игровой вселенной
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { bullets    = []
  , asteroids
    = initAsteroids asteroidsNumber (positions g) (directions g) (vectors g) (sizes g)
  , spaceship  = initSpaceship
  , background = initBackground
  }
  
-- | Инициализация фона
initBackground :: Background
initBackground = Background
   { backgroundPosition = (0, 0)
   , backgroundVelocity = (0, 0)
   }

-- | Инициализировать случайный бесконечный
-- список астероидов для игровой вселенной.
initAsteroids :: Int -> [Point] -> [Float] -> [Vector] -> [Float] ->[Asteroid]
initAsteroids 0 _ _ _ _ = []
initAsteroids n (p : positions) (d : directions) (v : velocities) (s : sizes)
  = initAsteroid p d v s : initAsteroids (n-1) positions directions velocities sizes

-- | Инициализировать один астероид.
initAsteroid :: Point -> Float -> Vector -> Float -> Asteroid
initAsteroid position direction velocity size = Asteroid 
  { asteroidPosition  = newPosition
  , asteroidDirection = direction
  , asteroidVelocity  = rotateV (direction * pi / 180) velocity
  , asteroidSize      = size
  , asteroidRadius    = size * 90
  }
  where
    (x, y) = position
    w      = fromIntegral screenWidth / 4
    h      = fromIntegral screenHeight / 4
    newX
      | x < 0 && x > - w = x - w
      | x > 0 && x <   w = x + w
      | otherwise        = x
    newY
      | y < 0 && y > - h = y - h
      | y > 0 && y <   h = y + h
      | otherwise        = y
    newPosition = (newX, newY)

-- | Начальное состояние корабля
initSpaceship :: Spaceship
initSpaceship = Spaceship
  { spaceshipPosition   = (0, 0)
  , spaceshipVelocity   = (0, 0) 
  , spaceshipAccelerate = 0
  , spaceshipAngularV   = 0
  , spaceshipDirection  = 0 
  , spaceshipSize       = 50
  }

-- | Инициализация пули
initBullet :: Universe -> Bullet
initBullet u = Bullet
    { bulletPosition  = spaceshipPosition (spaceship u)
	  + rotateV (spaceshipDirection (spaceship u) * pi / 180) (0, 60)
    , bulletVelocity  = rotateV 
	    ((spaceshipDirection (spaceship u)) * pi / 180) (0, 10)
    , bulletDirection = spaceshipDirection (spaceship u)
    , bulletSize      = 50
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
  { bullets  = initBullet u : bullets u }

-- =========================================
-- Обновление игровой вселенной
-- =========================================

-- | Обновить состояние игровой вселенной.
updateUniverse :: StdGen -> Float -> Universe -> Universe
updateUniverse g dt u 
  | isGameOver u = resetUniverse g u
  | otherwise = bulletsFaceAsteroids u
      { bullets    = updateBullets (bullets u)
      , asteroids  = updateAsteroids g (asteroids u) 
      , spaceship  = updateSpaceship (spaceship u)
      , background = updateBackground u
      }

-- | Столкновение пули с астероидами
bulletsFaceAsteroids :: Universe -> Universe
bulletsFaceAsteroids u = 
	bulletsFaceAsteroids2 u (asteroids u) (bullets u)
	
bulletsFaceAsteroids2 :: Universe -> [Asteroid] -> [Bullet] -> Universe
bulletsFaceAsteroids2 u a b 
  = u { asteroids = checkCollisions a b [] 
        , bullets = checkCollisionsForBulets a b }

checkCollisions :: [Asteroid] -> [Bullet] -> [Asteroid] -> [Asteroid]
checkCollisions [] _ newA = newA
checkCollisions a [] _ = a
checkCollisions (a:as) b newA 
	| (checkCollisions2 (asteroidPosition a) (asteroidSize a) b) /= b = checkCollisions as b newA 
	| otherwise = checkCollisions as b (a:newA)

checkCollisionsForBulets :: [Asteroid] -> [Bullet] -> [Bullet]
checkCollisionsForBulets [] b = b
checkCollisionsForBulets a [] = []
checkCollisionsForBulets (a:as) b 
  | (checkCollisions2 (asteroidPosition a) (asteroidSize a) b) == b = checkCollisionsForBulets as b 
  | otherwise = checkCollisions2 (asteroidPosition a) (asteroidSize a) b
	

checkCollisions2 :: Point -> Float -> [Bullet] -> [Bullet]
checkCollisions2 _ _ [] = []
checkCollisions2  pos rad (b:bs)
    | collision pos rad (bulletPosition b) (bulletSize b) = bs
	  | otherwise = [b] ++ (checkCollisions2 pos rad bs)


-- | Обновить состояние пуль
updateBullets :: [Bullet] -> [Bullet]
updateBullets bullets = filter visible (map updateBullet bullets)
  where
    visible bullet = (abs x) <= screenRight
	  && (abs y) <= screenUp
	where
	  (x, y)  = bulletPosition bullet

-- | Обновить состояние одной пули
updateBullet :: Bullet -> Bullet
updateBullet bullet = bullet
  { bulletPosition = newPosition }
  where
    newPosition = bulletPosition bullet + bulletVelocity bullet

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
updateShipPosition ship = (checkBoards (fst(spaceshipPosition ship)) (fst newPos) screenRight
                         , checkBoards (snd(spaceshipPosition ship)) (snd newPos) screenUp)
  where 
    newPos = (spaceshipPosition ship + spaceshipVelocity ship)

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
      = (checkBoards (fst (backgroundPosition (background u))) (fst newPos) screenRight
        , checkBoards (snd(backgroundPosition (background u))) (snd newPos) screenUp)
  , backgroundVelocity = - spaceshipVelocity (spaceship u)
  }
  where 
    newPos = (backgroundPosition (background u)) + (backgroundVelocity (background u))

-- | Обновить астероиды игровой вселенной.
updateAsteroids :: StdGen -> [Asteroid] -> [Asteroid]
updateAsteroids _ [] = []
updateAsteroids g asteroids 
  | length asteroids <= asteroidsNumber - 2
      = filter visible (map updateAsteroid asteroids)
        ++ initAsteroids 2 (positions g3) (directions g4) (vectors g5) (sizes g6)
  | otherwise =  filter visible (map updateAsteroid asteroids)
    where
      (g1,g2) = split g
      (g3,g4) = split g1
      (g5,g6) = split g2
      visible asteroid = abs x <= 2*screenRight + asteroidRadius asteroid
        && abs y <= 2*screenUp + asteroidRadius asteroid
	    where
	      (x, y)  = asteroidPosition asteroid

updateAsteroid :: Asteroid -> Asteroid 
updateAsteroid asteroid = asteroid
  { asteroidPosition = newPosition }
  where
    newPosition = asteroidPosition asteroid + asteroidVelocity asteroid

-- | Сбросить игру.
resetUniverse :: StdGen -> Universe -> Universe
resetUniverse g u = u
  { asteroids
      = initAsteroids asteroidsNumber (positions g) (directions g) (vectors g) (sizes g)
  , bullets    = []
  , spaceship  = initSpaceship
  }

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = spaceshipFaceAsteroids u || spaceshipFaceBullets u

-- | Определение столкновения корабля(кораблей) с астероидами
spaceshipFaceAsteroids :: Universe -> Bool -- 
spaceshipFaceAsteroids u = 
	spaceshipFaceAsteroids2 (spaceshipPosition (spaceship u)) (spaceshipSize (spaceship u)) (asteroids u)

spaceshipFaceAsteroids2 :: Point -> Float -> [Asteroid] -> Bool
spaceshipFaceAsteroids2 _ _ [] = False
spaceshipFaceAsteroids2 pos rad (a:as) = (collision pos rad (asteroidPosition a) (asteroidSize a)) 
						                 || (spaceshipFaceAsteroids2 pos rad as)

collision :: Point -> Float -> Point -> Float -> Bool
collision (x1, y1) r1 (x2, y2) r2 = d <= (r1 + r2)
	where d = sqrt((x1-x2)^2 + (y1-y2)^2)

-- Если будет мультиплеер с несколькими кораблями (а он, скорее всего, будет)
-- | Определение столкновения с пулей
spaceshipFaceBullets :: Universe -> Bool -- ??? -- Паше
spaceshipFaceBullets _ = False

-- =========================================
-- Константы, параметры игры
-- =========================================

-- | Количество астероидов
asteroidsNumber :: Int
asteroidsNumber = 50

-- | Ширина экрана.
screenWidth :: Int
screenWidth =  1366

-- | Высота экрана.
screenHeight :: Int
screenHeight = 768

-- | Положение верхнего края экрана.
screenUp :: Float
screenUp = fromIntegral screenHeight / 2

-- | Положение нижнего края экрана.
screenDown :: Float
screenDown = - fromIntegral screenHeight / 2

-- | Положение правого края экрана.
screenRight :: Float
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана.
screenLeft :: Float
screenLeft = - fromIntegral screenWidth / 2

-- | Скорость движения астероида по вселенной (в пикселях в секунду).
speed :: Float
speed = 100.0

-- | Небходимое расстояние между игроком и новоявленным астероидом.
playerOffset :: Float 
playerOffset = 300.0

-- | Расстояние между астероидами.
defaultOffset :: Float
defaultOffset = 150.0
