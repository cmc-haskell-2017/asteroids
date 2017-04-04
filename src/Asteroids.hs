module Asteroids where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line()
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

run :: Images -> IO ()
run images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) (handleUniverse g) updateUniverse
  where
    display = InWindow "Asteroids" (screenWidth, screenHeight) (150, 150)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just bullet      <- loadJuicyPNG "images/bullet.png"
  Just asteroid    <- loadJuicyPNG "images/asteroid.png"
  Just background' <- loadJuicyPNG "images/background.png"
  Just spaceship'  <- loadJuicyPNG "images/spaceship.png"
  Just table'      <- loadJuicyPNG "images/table.png"
  return Images
    { imageBullet     = scale 0.07 0.07 bullet
    , imageAsteroid   = scale  1.0  1.0 asteroid
    , imageBackground = scale  1.5  1.5 background'
    , imageSpaceship  = scale  0.2  0.2 spaceship'
    , imageTable      = scale  1.0  1.0 table'
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
  , imageTable      :: Picture -- ^ Заставка
  }

-- | Игровая вселенная
data Universe = Universe
  { asteroids      :: [Asteroid]  -- ^ Астероиды
  , spaceship      :: Spaceship   -- ^ Космический корабль
  , background     :: Background  -- ^ Фон
  , bullets        :: [Bullet]    -- ^ Пули
  , table          :: Maybe Table -- ^ Заставка
  , freshAsteroids :: [Asteroid]  -- ^ Бесконечный список "свежих" астероидов
  , score          :: Score       -- ^ Счёт
  }

-- | Заставка
data Table = Table 
  { tablePosition :: Point -- ^ Положение фона
  } deriving (Eq, Show)  

-- | Счёт.
type Score = Int

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
  , isfire              :: Bool   -- ^ Ведётся ли огонь?
  , fireReload          :: Int    -- ^ Счётчик перезарядки 
  } deriving (Eq, Show)

-- | Пуля
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
    width    = fromIntegral screenWidth
    height   = fromIntegral screenHeight

-- | Бесконечный список скоростей для астероидов
velocities :: StdGen -> [Vector]
velocities g
  = zipWith (\ x y -> (x, y)) (randomRs (0.5, 1.5) g1) (randomRs (0.5, 1.5) g2)
  where
    (g1, g2) = split g

-- | Бесконечный список направлений для астероидов
directions :: StdGen -> [Float]
directions g = randomRs (0.0, 360.0) g

-- | Бесконечный список размеров для астероидов
sizes :: StdGen -> [Float]
sizes g = randomRs (0.5, 1.0) g

-- | Бесконечный список астероидов
asteroidList :: [Point] -> [Float] -> [Vector] -> [Float] -> [Asteroid]
asteroidList (p : pos) (d : dir) (v : vel) (s : siz)
  = initAsteroid p d v s : asteroidList pos dir vel siz
asteroidList _ _ _ _ = []

-- | Инициализация астероида
initAsteroid :: Point -> Float -> Vector -> Float -> Asteroid
initAsteroid p d v s
  = Asteroid
    { asteroidPosition  = newP
    , asteroidDirection = d
    , asteroidVelocity  = rotateV (d * pi / 180) v
    , asteroidSize      = s
    }
    where
      (x, y) = p
      w      = screenRight
      h      = screenUp
      newX
        | x < 0 && x > - w = x - w
        | x > 0 && x <   w = x + w
        | otherwise        = x
      newY
        | y < 0 && y > - h = y - h
        | y > 0 && y <   h = y + h
        | otherwise        = y
      newP = (newX, newY)

-- | Инициализация игровой вселенной
initUniverse :: StdGen -> Universe
initUniverse g  = Universe
  { bullets        = []
  , asteroids      = take asteroidsNumber (initAsteroids g)
  , spaceship      = initSpaceship
  , background     = initBackground
  , table          = Nothing
  , freshAsteroids = drop asteroidsNumber (initAsteroids g)
  , score          = 0
  }
  
-- | Инициализация фона
initBackground :: Background
initBackground = Background
   { backgroundPosition = (0, 0)
   , backgroundVelocity = (0, 0)
   }

-- | Инициализация астероидов
initAsteroids :: StdGen -> [Asteroid]
initAsteroids g
  = asteroidList (positions g) (directions g) (velocities g) (sizes g)

-- | Начальное состояние корабля
initSpaceship :: Spaceship
initSpaceship = Spaceship
  { spaceshipPosition   = (0, 0)
  , spaceshipVelocity   = (0, 0) 
  , spaceshipAccelerate = 0
  , spaceshipAngularV   = 0
  , spaceshipDirection  = 0 
  , spaceshipSize       = 40
  , isfire              = False
  , fireReload          = 0
  }

-- | Инициализация пули
initBullet :: Spaceship -> Bullet
initBullet ship = Bullet
    { bulletPosition  = spaceshipPosition ship
        + rotateV (spaceshipDirection ship * pi / 180) (0, 60)
    , bulletVelocity  = rotateV 
      (spaceshipDirection ship * pi / 180) (0, 10)
    , bulletDirection = spaceshipDirection ship
    , bulletSize      = 15
    }

-- | Инициализация заставки
initTable :: Table
initTable = Table { tablePosition = (0, 0) }

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
  , drawTable      (imageTable images)      (table u)
  , drawScore      (score u)
  ]

-- | Нарисовать счёт в левом верхнем углу экрана.
drawScore :: Score -> Picture
drawScore s = translate (-w) h (scale 10 10 (pictures
  [ color white (polygon [ (0, 0), (0, -6), (15, -6), (15, 0) ])          -- белая рамка
  , color black (polygon [ (0, 0), (0, -5.9), (14.9, -5.9), (14.9, 0) ])    -- чёрные внутренности
  , translate 4 (-4.5) (scale 0.03 0.03 (color red (text (show s))))  -- красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Отобразить список астероидов
drawAsteroids :: Picture -> [Asteroid] -> Picture
drawAsteroids image asteroids' = foldMap (drawAsteroid image) asteroids'

-- | Отобразить астероид
drawAsteroid :: Picture -> Asteroid -> Picture 
drawAsteroid image asteroid
  = translate x y (resize (rotate (- asteroidDirection asteroid) image))
  where
    size   = asteroidSize asteroid
    resize = scale size size
    (x, y) = asteroidPosition asteroid

-- | Отобразить фон.
drawBackground :: Picture -> Background -> Picture
drawBackground image background' = translate x y image
  where
    (x, y) = backgroundPosition background'

-- | Отобразить корабль.
drawSpaceship :: Picture -> Spaceship -> Picture
drawSpaceship image spaceship'
  = translate x y (rotate (- spaceshipDirection spaceship') image)
  where
    (x, y) = spaceshipPosition spaceship'

-- | Отобразить пули.
drawBullets :: Picture -> [Bullet] -> Picture
drawBullets _     []       = blank
drawBullets image bullets' = foldMap (drawBullet image) bullets'

-- | Отобразить пулю.
drawBullet :: Picture -> Bullet -> Picture
drawBullet image bullet =
  translate x y (rotate (- bulletDirection bullet) image)
  where
    (x, y) = bulletPosition bullet

-- | Отобразить заставку 
drawTable :: Picture -> Maybe Table -> Picture
drawTable _     Nothing       = blank
drawTable image (Just table') = translate x y image
  where
    (x, y) = tablePosition table'

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработчик событий игры.
handleUniverse :: StdGen -> Event -> Universe -> Universe
handleUniverse _ (EventKey (SpecialKey KeyUp) Down _ _)    u
  = u { spaceship = moveShip 0.1 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyDown) Down _ _)  u
  = u { spaceship = moveShip (-0.1) (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyUp) Up _ _)      u
  = u { spaceship = moveShip 0 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyDown) Up _ _)    u
  = u { spaceship = moveShip 0 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyLeft) Down _ _)  u
  = u { spaceship = turnShip 5 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyRight) Down _ _) u
  = u { spaceship = turnShip (-5) (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyLeft) Up _ _)    u
  = u { spaceship = turnShip 0 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeyRight) Up _ _)   u
  = u { spaceship = turnShip 0 (spaceship u) }
handleUniverse _ (EventKey (SpecialKey KeySpace) Down _ _) u
  = u { spaceship = (spaceship u){isfire = True}}
handleUniverse _ (EventKey (SpecialKey KeySpace) Up _ _) u
  = u { spaceship = (spaceship u){isfire = False}}  -- bullets = fireSpaceship (spaceship u) (bullets u) }
handleUniverse g (EventKey (SpecialKey KeyEnter) Down _ _) u
  = resetUniverse g u
handleUniverse _ _ u = u

-- | Движение корабля
moveShip :: Float -> Spaceship -> Spaceship
moveShip a ship = ship { spaceshipAccelerate = a }

-- | Поворот корабля
turnShip :: Float -> Spaceship -> Spaceship
turnShip a ship = ship { spaceshipAngularV = a }

-- | Выстрел корабля
fireSpaceship :: Spaceship -> [Bullet] -> [Bullet]
fireSpaceship ship bs = initBullet ship : bs

-- =========================================
-- Обновление игровой вселенной
-- =========================================

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse _ u 
  | isGameOver u = u { table = Just initTable } -- resetUniverse g u
  | otherwise = bulletsFaceAsteroids u
      { bullets        = updateBullets newBullets
      , asteroids
        = updateAsteroids (asteroids u) (head (freshAsteroids u))
      , spaceship      = updateSpaceship (spaceship u)
      , background     = updateBackground u
      , freshAsteroids = tail (freshAsteroids u)
      }
      where
        newBullets
          | isfire (spaceship u) && fireReload (spaceship u) == reloadTime
            = fireSpaceship (spaceship u) (bullets u)
          | otherwise = bullets u

-- | Столкновение пуль с астероидами
bulletsFaceAsteroids :: Universe -> Universe
bulletsFaceAsteroids u = u
  { asteroids = newA
  , bullets   = newB
  , score     = score u + length b - length newB
  }
  where
    a    = asteroids u
    b    = bullets u
    newA = filter (not . asteroidFaceBullets b) a
    newB = filter (not . bulletFaceAsteroids a) b

-- | Астероид сталкивается с пулями?
asteroidFaceBullets :: [Bullet] -> Asteroid -> Bool
asteroidFaceBullets [] _ = False
asteroidFaceBullets bs a = any (asteroidFaceBullet a) bs

-- | Астероид сталкивается с пулей?
asteroidFaceBullet :: Asteroid -> Bullet -> Bool
asteroidFaceBullet a b = collision aPos aRad bPos bRad
  where 
    aPos = asteroidPosition a
    aRad = asteroidSize a * 70
    bPos = bulletPosition b
    bRad = bulletSize b

-- | Пуля сталкивается с астероидами?
bulletFaceAsteroids :: [Asteroid] -> Bullet -> Bool
bulletFaceAsteroids [] _ = False
bulletFaceAsteroids as b = any (bulletFaceAsteroid b) as

-- | Пуля сталкивается с астероидом?
bulletFaceAsteroid :: Bullet -> Asteroid -> Bool
bulletFaceAsteroid b a = collision aPos aRad bPos bRad
  where 
    aPos = asteroidPosition a
    aRad = asteroidSize a * 70
    bPos = bulletPosition b
    bRad = bulletSize b

-- | Обновить состояние пуль
updateBullets :: [Bullet] -> [Bullet]
updateBullets bullets' = filter visible (map updateBullet bullets')
  where
    visible bullet = (abs x) <= screenRight && (abs y) <= screenUp
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
updateSpaceship ship = ship { 
    spaceshipPosition  = updateShipPosition ship
  , spaceshipVelocity  = updateShipVelocity ship
  , spaceshipDirection = (if newDir > 180 then (-1) else (if newDir < -180 then (1) else 0))*360 + newDir 
  , fireReload = if (fireReload ship) == reloadTime then 0 else (fireReload ship) + 1 
  }
  where
    newDir = spaceshipDirection ship + spaceshipAngularV ship

-- | Обновление положения корабля
updateShipPosition :: Spaceship -> Point
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
updateAsteroids :: [Asteroid] -> Asteroid -> [Asteroid]
updateAsteroids [] _  = []
updateAsteroids asteroids' a
  | length asteroids' < asteroidsNumber
      = a : filter visible (map updateAsteroid asteroids')
  | otherwise =  filter visible (map updateAsteroid asteroids')
    where
      visible asteroid = abs x <= 2 * screenRight + radius
        && abs y <= 2 * screenUp + radius
        where
        (x, y)  = asteroidPosition asteroid
        radius  = asteroidSize asteroid * 70

-- | Обновить астероид
updateAsteroid :: Asteroid -> Asteroid 
updateAsteroid asteroid = asteroid
  { asteroidPosition = newPosition }
  where
    newPosition = asteroidPosition asteroid + asteroidVelocity asteroid

-- | Сбросить игру.
resetUniverse :: StdGen -> Universe -> Universe
resetUniverse g _ = initUniverse g

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = spaceshipFaceAsteroids (spaceship u) (asteroids u)
  || spaceshipFaceBullets (spaceship u) (bullets u)

-- | Определение столкновения корабля с астероидами
spaceshipFaceAsteroids :: Spaceship -> [Asteroid] -> Bool
spaceshipFaceAsteroids ship as = any (spaceshipFaceAsteroid ship) as

-- | Определение столкновения корабля с астероидом
spaceshipFaceAsteroid :: Spaceship -> Asteroid -> Bool
spaceshipFaceAsteroid ship a = collision shipPos shipRad aPos aRad
  where
    shipPos = spaceshipPosition ship
    shipRad = spaceshipSize ship
    aPos    = asteroidPosition a
    aRad    = asteroidSize a * 70

-- | Определение пересечения двух окружностей
collision :: Point -> Float -> Point -> Float -> Bool
collision (x1, y1) r1 (x2, y2) r2 = d <= (r1 + r2)
  where
    dx = x1 - x2
    dy = y1 - y2
    d  = sqrt(dx^2 + dy^2)

-- | Определение столкновения с пулями
spaceshipFaceBullets :: Spaceship -> [Bullet] -> Bool
spaceshipFaceBullets ship bs = any (spaceshipFaceBullet ship) bs

-- | Определение столкновения с пулей
spaceshipFaceBullet :: Spaceship -> Bullet -> Bool
spaceshipFaceBullet ship b = collision shipPos shipRad bPos bRad
  where
    shipPos = spaceshipPosition ship
    shipRad = spaceshipSize ship
    bPos    = bulletPosition b
    bRad    = bulletSize b

-- =========================================
-- Константы, параметры игры
-- =========================================

-- | Количество астероидов
asteroidsNumber :: Int
asteroidsNumber = 100

-- | Время перезарядки
reloadTime :: Int
reloadTime = 10

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
