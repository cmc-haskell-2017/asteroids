module Models where

import Graphics.Gloss.Interface.Pure.Game

-- | Изображения объектов.
data Images = Images
  { imageBullet     :: Picture -- ^ Ракета.
  , imageAsteroid   :: Picture -- ^ Aстероид.
  , imageBackground :: Picture -- ^ Фон.
  , imageSpaceship  :: Picture -- ^ Корабль.
  , imageTable      :: Picture -- ^ Заставка
  }

-- | Заставка
data Table = Table 
  { tablePosition :: Point -- ^ Положение фона
  }  

-- | Счёт.
type Score = Int

-- | Фон
data Background = Background
  { backgroundPosition :: Point  -- ^ Положение фона
  , backgroundVelocity :: Vector -- ^ Вектор скорости фона
  }


-- | Космический корабль
data Spaceship = Spaceship
  { spaceshipPosition   :: Point  -- ^ Положение корабля
  , spaceshipVelocity   :: Vector -- ^ Скорость корабля
  , spaceshipAccelerate :: Float  -- ^ Ускорение
  , spaceshipDirection  :: Float  -- ^ Направление корабля
  , spaceshipAngularV   :: Float  -- ^ Угловая скорость
  , spaceshipSize       :: Float  -- ^ Размер корабля
  , isfire              :: Bool   -- ^ Ведётся ли огонь?
  , fireReload          :: Float    -- ^ Счётчик перезарядки 
  }

-- | Пуля
data Bullet = Bullet
  { bulletPosition  :: Point  -- ^ Положение пули
  , bulletVelocity  :: Vector -- ^ Скорость пули
  , bulletDirection :: Float  -- ^ Направление пули
  , bulletSize      :: Float  -- ^ Размер пули
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

-- | Астероид
data Asteroid = Asteroid
  { asteroidPosition  :: Point  -- ^ Положение астероида
  , asteroidDirection :: Float  -- ^ Направление астероида
  , asteroidVelocity  :: Vector -- ^ Скорость астероида
  , asteroidSize      :: Float  -- ^ Размер астероида
  }

