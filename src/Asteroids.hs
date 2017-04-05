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
  { imageBullet     :: Picture -- ^ Ракета
  , imageAsteroid   :: Picture -- ^ Aстероид
  , imageBackground :: Picture -- ^ Фон
  , imageSpaceship  :: Picture -- ^ Корабль
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
  { tablePosition :: Point -- ^ Положение заставки
  }

-- | Счёт.
type Score = Int

-- | Фон
data Background = Background
  { backgroundPosition :: Point  -- ^ Положение фона
  , backgroundVelocity :: Vector -- ^ Вектор скорости фона
  }

-- | Астероид
data Asteroid = Asteroid
  { asteroidPosition  :: Point  -- ^ Положение астероида
  , asteroidDirection :: Float  -- ^ Направление астероида
  , asteroidVelocity  :: Vector -- ^ Скорость астероида
  , asteroidSize      :: Float  -- ^ Размер астероида
  }

-- | Космический корабль
data Spaceship = Spaceship
  { spaceshipPosition   :: Point  -- ^ Положение корабля
  , spaceshipVelocity   :: Vector -- ^ Скорость корабля
  , spaceshipAccelerate :: Float  -- ^ Ускорение
  , spaceshipDirection  :: Float  -- ^ Направление корабля
  , spaceshipAngularV   :: Float  -- ^ Угловая скорость
  , spaceshipSize       :: Float  -- ^ Размер корабля
  , isFire              :: Bool   -- ^ Ведётся ли огонь?
  , fireReload          :: Float  -- ^ Счётчик перезарядки 
  }

-- | Пуля
data Bullet = Bullet
  { bulletPosition  :: Point  -- ^ Положение пули
  , bulletVelocity  :: Vector -- ^ Скорость пули
  , bulletDirection :: Float  -- ^ Направление пули
  , bulletSize      :: Float  -- ^ Размер пули
  }

-- | Бесконечный список векторов
vectors :: (Float, Float) -> (Float, Float) -> StdGen -> [Vector]
vectors ran1 ran2 g
  = zipWith (\ x y -> (x, y)) (randomRs ran1 g1) (randomRs ran2 g2)
  where
    (g1, g2) = split g

-- | Бесконечный список чисел
floats :: (Float, Float) -> StdGen -> [Float]
floats range g = randomRs range g

-- | Бесконечный список астероидов
asteroidList :: [Point] -> [Float] -> [Vector] -> [Float] -> [Asteroid]
asteroidList (p : pos) (d : dir) (v : vel) (s : siz)
  = initAsteroid p d v s : asteroidList pos dir vel siz
asteroidList _ _ _ _ = []

-- | Инициализация астероида
initAsteroid :: Point -> Float -> Vector -> Float -> Asteroid
initAsteroid pos dir vel siz
  = Asteroid
    { asteroidPosition  = newPos
    , asteroidDirection = dir
    , asteroidVelocity  = rotateV (dir * pi / 180) vel
    , asteroidSize      = siz
    }
    where
      (x, y) = pos
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
      newPos = (newX, newY)

-- | Инициализация игровой вселенной
initUniverse :: StdGen -> Universe
initUniverse g = Universe
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
initAsteroids g = asteroidList (vectors xPositions yPositions g)
                               (floats directions g)
                               (vectors velocities velocities g)
                               (floats sizes g)

-- | Начальное состояние корабля
initSpaceship :: Spaceship
initSpaceship = Spaceship
  { spaceshipPosition   = (0, 0)
  , spaceshipVelocity   = (0, 0) 
  , spaceshipAccelerate = 0
  , spaceshipAngularV   = 0
  , spaceshipDirection  = 0 
  , spaceshipSize       = 40
  , isFire              = False
  , fireReload          = 0
  }

-- | Инициализация пули
initBullet :: Spaceship -> Bullet
initBullet ship = Bullet
    { bulletPosition  = spaceshipPosition ship
        + rotateV (spaceshipDirection ship * pi / 180) (0, 60)
    , bulletVelocity  = rotateV (spaceshipDirection ship * pi / 180) (0, 10)
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
    w = screenRight
    h = screenUp

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
  = u { spaceship = (spaceship u){isFire = True}}
handleUniverse _ (EventKey (SpecialKey KeySpace) Up _ _) u
  = u { spaceship = (spaceship u){isFire = False}}  -- bullets = fireSpaceship (spaceship u) (bullets u) }
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
updateUniverse dt u 
  | isGameOver u = u { table = Just initTable } -- resetUniverse g u
  | otherwise = bulletsFaceAsteroids u
      { bullets        = updateBullets t newBullets
      , asteroids      = updateAsteroids t newAsteroids
      , spaceship      = updateSpaceship t (spaceship u)
      , background     = updateBackground t u
      , freshAsteroids = tail (freshAsteroids u)
      }
      where
        t = 60 * dt
        newAsteroids
          | length (asteroids u) < asteroidsNumber
            = head (freshAsteroids u) : asteroids u
          | otherwise = asteroids u
        newBullets
          | isFire (spaceship u) && fireReload (spaceship u) == reloadTime
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
updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets t bullets' = filter visible (map (updateBullet t) bullets')
  where
    visible bullet = abs x <= screenRight && abs y <= screenUp
     where
      (x, y)  = bulletPosition bullet

-- | Обновить состояние одной пули
updateBullet :: Float -> Bullet -> Bullet
updateBullet t bullet = bullet
  { bulletPosition = newPosition }
  where
    newPosition = bulletPosition bullet + mulSV t (bulletVelocity bullet)

-- | Обновить состояние корабля.
updateSpaceship :: Float -> Spaceship -> Spaceship
updateSpaceship t ship = ship
  { spaceshipPosition  = updateShipPosition t ship
  , spaceshipVelocity  = updateShipVelocity t ship
  , spaceshipDirection = newDir
  , fireReload         = newReload
  }
  where
    shipDir = spaceshipDirection ship + t * spaceshipAngularV ship
    newDir
      | shipDir >  180 = shipDir - 360
      | shipDir < -180 = shipDir + 360
      | otherwise      = shipDir
    newReload
      | fireReload ship == reloadTime = 0
      | otherwise = fireReload ship + t

-- | Обновление положения корабля
updateShipPosition :: Float -> Spaceship -> Point
updateShipPosition t ship
  = ( checkBoards (fst(spaceshipPosition ship)) (fst newPos) screenRight
    , checkBoards (snd(spaceshipPosition ship)) (snd newPos) screenUp )
  where 
    newPos = spaceshipPosition ship + mulSV t (spaceshipVelocity ship)

-- | Проверка выхода за границы
checkBoards :: Float -> Float -> Float -> Float
checkBoards x y z
    | x >= 0    = min y (  z)
    | otherwise = max y (- z)
    
-- | Обновление скорости корабля
updateShipVelocity :: Float -> Spaceship -> Vector
updateShipVelocity t ship = velocity + mulSV t acceleration
  where
    acceleration = mulSV (spaceshipAccelerate ship)
      (unitVectorAtAngle ((90 + spaceshipDirection ship) * pi / 180))
    velocity = damping * cross * spaceshipVelocity ship
      where
        cross
          | crossX && crossY = (-1, -1)
          | crossX           = (-1,  1)
          | crossY           = ( 1, -1)
          | otherwise        = ( 1,  1)
          where
            crossX = abs (fst(updateShipPosition t ship)) == screenRight
            crossY = abs (snd(updateShipPosition t ship)) == screenUp

-- | Обновить фон
updateBackground :: Float -> Universe -> Background
updateBackground t u = Background
  { backgroundPosition 
      = (checkBoards (fst (backgroundPosition (background u))) (fst newPos) screenRight
        , checkBoards (snd(backgroundPosition (background u))) (snd newPos) screenUp)
  , backgroundVelocity = - spaceshipVelocity (spaceship u)
  }
  where 
    newPos = backgroundPosition (background u)
      + mulSV t (backgroundVelocity (background u))

-- | Обновить астероиды игровой вселенной.
updateAsteroids :: Float -> [Asteroid] -> [Asteroid]
updateAsteroids t asteroids'
  = filter visible (map (updateAsteroid t) asteroids')
  where
    visible asteroid = abs x <= 2 * screenRight + radius
      && abs y <= 2 * screenUp + radius
      where
      (x, y)  = asteroidPosition asteroid
      radius  = asteroidSize asteroid * 70

-- | Обновить астероид
updateAsteroid :: Float -> Asteroid -> Asteroid 
updateAsteroid t asteroid = asteroid
  { asteroidPosition = newPosition }
  where
    newPosition = asteroidPosition asteroid
      + mulSV t (asteroidVelocity asteroid)

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

-- | Интервал позиций по оси абсцисс для астероидов
xPositions :: (Float, Float)
xPositions = (- width, width)
  where
    width = fromIntegral screenWidth

-- | Интервал позиций по оси ординат для астероидов
yPositions :: (Float, Float)
yPositions = (- height, height)
  where
    height = fromIntegral screenHeight

-- | Интервал скоростей для астероидов
velocities :: (Float, Float)
velocities = (0.0, 2.0)

-- | Интервал направлений для астероидов
directions :: (Float, Float)
directions = (0.0, 360.0)

-- | Интервал размеров для астероидов
sizes :: (Float, Float)
sizes = (0.5, 1.0)

-- | Затухание скорости корабля
damping :: (Float, Float)
damping = (0.98, 0.98)

-- | Время перезарядки
reloadTime :: Float
reloadTime = 10.0

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
