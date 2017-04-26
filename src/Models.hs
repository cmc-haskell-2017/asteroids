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

-- | Режим корабля
data Mode = Bot | Player deriving (Eq)

-- | Космический корабль
data Spaceship = Spaceship
  { spaceshipID         :: Int        -- ^ Имя корабля
  , spaceshipMode       :: Mode       -- ^ Режим корабля (бот/игрок)
  , lastAction          :: ShipAction -- ^ Последнее действие корабля
  , spaceshipPosition   :: Point      -- ^ Положение корабля
  , spaceshipVelocity   :: Vector     -- ^ Скорость корабля
  , spaceshipAccelerate :: Float      -- ^ Ускорение
  , spaceshipDirection  :: Float      -- ^ Направление корабля
  , spaceshipAngularV   :: Float      -- ^ Угловая скорость
  , spaceshipSize       :: Float      -- ^ Размер корабля
  , isfire              :: Bool       -- ^ Ведётся ли огонь?
  , fireReload          :: Float      -- ^ Счётчик перезарядки 
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
  , spaceships     :: [Spaceship] -- ^ Космический корабль
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


-- | Поворот корабля
data RotateAction = ToLeft | ToRight deriving(Eq)

-- | Направление ускорения корабля
data EngineAction = Forward | Back deriving(Eq)

-- | Действие корабля
data ShipAction = ShipAction
  { shipID       :: Int
  , rotateAction :: Maybe RotateAction
  , engineAction :: Maybe EngineAction
  , fireAction   :: Bool
  }


-- | Моноид для действий корабля
instance Monoid ShipAction where
    mempty = ShipAction {
        shipID = 0
      , rotateAction = Nothing
      , engineAction = Nothing
      , fireAction   = False
      }
    mappend f g = ShipAction {
        shipID = (shipID f)
      , rotateAction = rw (rotateAction f) (rotateAction g)
      , engineAction = rw (engineAction f) (engineAction g)
      , fireAction   = (fireAction f) || (fireAction g)
      }
    
-- | Сложение двух поворотов или направлений
rw :: Eq a => Maybe a -> Maybe a -> Maybe a
rw Nothing g  = g
rw f Nothing  = f
rw f g
  | f == g  = f
  | otherwise = Nothing
