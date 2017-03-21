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
    display = InWindow "Asteroids" (screenWidth, screenHeight) (100, 100)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  -- Just asteroid   <- loadJuicyPNG "images/asteroid.png"
  Just background  <- loadJuicyPNG "images/background.png"
  return Images
    { -- imageAsteroid   = scale 0.1 0.1 asteroid
      imageBackground  = scale 1 1 background
    }


-- =========================================
-- Модель игровой вселенной
-- =========================================

-- | Изображения объектов.
data Images = Images
  { -- imageAsteroid  :: Picture   -- ^ Изображение астероида.
   imageBackground :: Picture   -- ^ Фон.
  }

-- | Игровая вселенная
data Universe = Universe
 { asteroids :: [Asteroid] -- ^ Астероиды
 , spaceship :: Spaceship -- ^ Космический корабль
 , background :: Background -- ^ Фон
 -- , fireballs :: [Fireball] -- Паш, для этого тебе тип надо сделать
 }

-- | Фон
data Background = Background
 { backgroundPosition :: Point -- ^ Положение фона
 } deriving (Eq, Show)

-- | Астероид
data Asteroid = Asteroid
 { asteroidPosition :: Point -- ^ Положение астероида
 , asteroidVelocity :: Vector -- ^ Скорость астероида
 , asteroidSize :: Float -- ^ Размер астероида (Радиус/Диаметр)
 } deriving (Eq, Show)

-- | Космический корабль
data Spaceship = Spaceship
 { spaceshipPosition :: Point -- ^ Положение корабля
 , spaceshipVelocity :: Vector -- ^ Скорость корабля
 , spaceshipAccelerate :: Float -- ^ Ускорение
 , spaceshipDirection :: Float --  ^ Направление корабля
 , spaceshipAngularV :: Float -- ^ Угловая скорость
 , spaceshipSize :: Float -- ^ Размер корабля
 } deriving (Eq, Show)

-- Тут тип пули (одной) будет:

-- | Инициализация игровой вселенной.
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { -- asteroids  = initAsteroids g
    spaceship = initSpaceship
   ,background = initBackground
  }
  
-- | Начальное состояние корабля.
initSpaceship :: Spaceship
initSpaceship = Spaceship
  { spaceshipPosition = (0, 0)
  , spaceshipVelocity = (0,0) 
  , spaceshipAccelerate = 0
  , spaceshipAngularV = 0
  , spaceshipDirection = 0 
  , spaceshipSize = 1
  }

-- | Инициализация фона.
initBackground :: Background
initBackground = Background
   { backgroundPosition = (0, 0)
   }
  
  -- | Инициализировать один астероид.
-- initAsteroid :: Point -> Asteroid
-- initAsteroid a = a-- ???

-- | Инициализировать случайный бесконечный
-- список астероидов для игровой вселенной.
-- initAsteroids :: StdGen -> [Asteroid]
-- initAsteroids a = map initAsteroid
  --(??? a)

-- Инициализация для пуль, хз как лучше,  наверное как и астероиды?

  -- =========================================
-- Отрисовка игровой вселенной
-- =========================================

-- | Отобразить игровую вселенную.
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  [ -- drawAsteroids  (asteroids u)
    drawBackground  (imageBackground  images) (background  u)
    ,drawSpaceship (spaceship u) 
  ]
  
--drawAsteroids :: -- ??? Тимуру

-- | Отобразить фон.
drawBackground :: Picture -> Background -> Picture
drawBackground image background = translate x y image
  where
    (x, y) = backgroundPosition background

drawSpaceship :: Spaceship -> Picture
drawSpaceship spaceship = color green drawShip
  where
    drawShip = pictures (map polygon (shipPolygons spaceship))


shipPolygons :: Spaceship -> [Path]
shipPolygons ship = map (map move)
  [ [ (0, 1600), (400, -100), (-400, -100) ]
  , [ (200, 0), (800, -450), (-800, -450), (-200, 0) ]
  , [ (800, 600), (800, -650), (650, -450) ]
  , [ (-800, 600), (-800, -650), (-650, -450) ] 
  , [ (-300, -450), (300, -450), (450, -700), (-450, -700) ] ]
  where
    move (x, y) = spaceshipPosition ship + mulSV 0.03 (rotateV ((spaceshipDirection ship) * pi / 180) (x, y))

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработчик событий игры.
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeyUp) Down _ _) =  moveShip 0.1
handleUniverse (EventKey (SpecialKey KeyDown) Down _ _) = moveShip (-0.1)
handleUniverse (EventKey (SpecialKey KeyUp) Up _ _) = moveShip 0 
handleUniverse (EventKey (SpecialKey KeyDown) Up _ _) = moveShip 0 
handleUniverse (EventKey (SpecialKey KeyLeft) Down _ _) = turnShip 5 
handleUniverse (EventKey (SpecialKey KeyRight) Down _ _) = turnShip (-5) 
handleUniverse (EventKey (SpecialKey KeyLeft) Up _ _) = turnShip 0 
handleUniverse (EventKey (SpecialKey KeyRight) Up _ _) = turnShip 0 
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) = id -- fireSpaceship 
handleUniverse _ = id

moveShip :: Float -> Universe -> Universe
moveShip a u = u 
 { spaceship = (spaceship u) {spaceshipAccelerate = a }
 }

turnShip :: Float -> Universe -> Universe
turnShip a u = u 
 { spaceship = (spaceship u) {spaceshipAngularV = a }
 }

--fireSpaceship :: -- ??? Паш, твоё, тут как раз появляются пули и летят по направлению корабля 

-- =========================================
-- Обновление игровой вселенной
-- =========================================

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u  
  | False = u -- isGameOver u = resetUniverse u
  | otherwise = u
   { -- asteroids  = updateAsteroids  dt (asteroids  u)
    spaceship = updateSpaceship dt (spaceship u)
   }
 --where
 -- ??? тут почти у всех

-- | Обновить состояние корабля.
updateSpaceship :: Float -> Spaceship -> Spaceship
updateSpaceship dt spaceship =  spaceship 
	{
		spaceshipPosition = ((checkWidth spaceship),(checkHeight spaceship))
		, spaceshipVelocity = 
			(((if w  then (-1) else 1) * (fst(spaceshipVelocity spaceship))),((if h  then (-1) else 1) * (snd(spaceshipVelocity spaceship))))  +  mul 
		, spaceshipDirection = if newDir > 180 then newDir - 360
								else if newDir < -180 then newDir + 360 else newDir
	}
	where 
		h = (checkHeight spaceship) == (fromIntegral screenHeight / 2) || (checkHeight spaceship) == (-1) * (fromIntegral screenHeight / 2)  
		w = (checkWidth spaceship) == (fromIntegral screenWidth / 2) || (checkWidth spaceship) == (-1) * (fromIntegral screenWidth / 2)
		newDir = spaceshipDirection spaceship + (spaceshipAngularV spaceship)
		mul = mulSV (spaceshipAccelerate spaceship) (unitVectorAtAngle (((spaceshipDirection spaceship) + 90) * pi / 180)) 

checkHeight :: Spaceship -> Float
checkHeight ship 
	| (snd (spaceshipPosition ship)) >= 0 = min (fromIntegral screenHeight / 2) (snd (spaceshipPosition ship + spaceshipVelocity ship))
	| otherwise = max (-1 * (fromIntegral screenHeight / 2)) (snd (spaceshipPosition ship + spaceshipVelocity ship))

checkWidth :: Spaceship -> Float
checkWidth ship 
	| (fst (spaceshipPosition ship)) >= 0 = min (fromIntegral screenWidth / 2) (fst (spaceshipPosition ship + spaceshipVelocity ship))
	| otherwise = max (-1 * (fromIntegral screenWidth / 2)) (fst (spaceshipPosition ship + spaceshipVelocity ship))

-- | Обновить астероиды игровой вселенной.
updateAsteroids :: Float -> [Asteroid] -> [Asteroid]
updateAsteroids _ [] = []
-- updateAsteroids -- ??? Тимур

-- | Сбросить игру.
resetUniverse :: Universe -> Universe
resetUniverse u = u
  { asteroids  = tail (asteroids u)
  , spaceship = initSpaceship
  }

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = spaceshipFaceAsteroids
  where
    spaceshipFaceAsteroids = False-- ??? - Паше

-- spaceshipFaceAsteroid -- Паше
-- =========================================
-- Константы, параметры игры
-- =========================================

-- | Ширина экрана.
screenWidth :: Int
screenWidth = 1600

-- | Высота экрана.
screenHeight :: Int
screenHeight = 900

-- | Положение правого края экрана.
screenRight :: Float
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана.
screenLeft :: Float
screenLeft = - fromIntegral screenWidth / 2
