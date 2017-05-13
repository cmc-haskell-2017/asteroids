{-# LANGUAGE DeriveGeneric #-}
module Models where

import Data.ByteString.Lazy.Internal()
import Data.Binary
import GHC.Generics
import Graphics.Gloss.Interface.Pure.Game

import Network.WebSockets

-- | Изображения объектов.
data Images = Images
  { imageBullet     :: Picture -- ^ Ракета.
  , imageAsteroid   :: Picture -- ^ Aстероид.
  , imageBackground :: Picture -- ^ Фон.
  , imageSpaceship  :: Picture -- ^ Корабль.
  , imageTable      :: Picture -- ^ Заставка
  , imageBonus1     :: Picture -- ^ Бонус-топливо
  , imageBonus2     :: Picture -- ^ Бонус-замедление
  , imageBonus3     :: Picture -- ^ Бонус-ускорение
  , imageBonus4     :: Picture -- ^ Бонус-оружие
  }

-- | Заставка
data Table = Table 
  { tablePosition :: Point -- ^ Положение фона
  } deriving (Generic)

instance Binary Table

-- | Счёт.
type Score = Int

-- | Идентификатор игрока
type PlayerID = Int

-- | Фон
data Background = Background
  { backgroundPosition :: Point  -- ^ Положение фона
  , backgroundVelocity :: Vector -- ^ Вектор скорости фона
  } deriving (Generic)

instance Binary Background

-- | Режим корабля
data Mode = Bot | Player deriving (Eq, Generic)

instance Binary Mode

-- | Космический корабль
data Spaceship = Spaceship
  { spaceshipID         :: PlayerID   -- ^ Имя корабля
  , spaceshipMode       :: Mode       -- ^ Режим корабля (бот/игрок)
  , lastAction          :: ShipAction -- ^ Последнее действие корабля
  , group               :: Int        -- ^ Группа, за которую играет корабль
  , spaceshipPosition   :: Point      -- ^ Положение корабля
  , spaceshipVelocity   :: Vector     -- ^ Скорость корабля
  , spaceshipAccelerate :: Float      -- ^ Ускорение
  , spaceshipDirection  :: Float      -- ^ Направление корабля
  , spaceshipAngularV   :: Float      -- ^ Угловая скорость
  , spaceshipSize       :: Float      -- ^ Размер корабля
  , shipLife            :: Float      -- ^ Кол-ва топлива у корабля
  , isfire              :: Bool       -- ^ Ведётся ли огонь?
  , fireReload          :: Float      -- ^ Счётчик перезарядки 
  , bonIndex            :: (Int, Float) -- ^ Номер действующего индекса
  } deriving (Generic)

instance Binary Spaceship

-- | Пуля
data Bullet = Bullet
  { bulletPosition  :: Point  -- ^ Положение пули
  , bulletVelocity  :: Vector -- ^ Скорость пули
  , bulletDirection :: Float  -- ^ Направление пули
  , bulletSize      :: Float  -- ^ Размер пули
  } deriving (Generic)

instance Binary Bullet

-- | Игровая вселенная
data Universe = Universe
  { asteroids      :: [Asteroid]  -- ^ Астероиды
  , spaceships     :: [Spaceship] -- ^ Космический корабль
  , bonuses        :: [Bonus]     -- ^ Бонусы
  , playerID       :: PlayerID    -- ^ Идентификатор игрока
  , background     :: Background  -- ^ Фон
  , bullets        :: [Bullet]    -- ^ Пули
  , table          :: Maybe Table -- ^ Заставка
  , freshPositions :: [Point]     -- ^ Бесконечный список "свежих" позиций для кораблей
  , freshAsteroids :: [Asteroid]  -- ^ Бесконечный список "свежих" астероидов
  , freshBonuses   :: [Bonus]     -- ^ Бесконечный список "свежих" бонусов
  , score          :: Score       -- ^ Счёт
  } deriving (Generic)

instance Binary Universe

instance WebSocketsData Universe where
  fromLazyByteString = decode
  toLazyByteString   = encode

-- | Астероид
data Asteroid = Asteroid
  { asteroidPosition  :: Point  -- ^ Положение астероида
  , asteroidDirection :: Float  -- ^ Направление астероида
  , asteroidVelocity  :: Vector -- ^ Скорость астероида
  , asteroidSize      :: Float  -- ^ Размер астероида
  } deriving (Generic)

instance Binary Asteroid

-- | Бонус
data Bonus = Bonus
  { bonusNumber    :: Int    -- ^ Номер бонуса
  , bonusPosition  :: Point  -- ^ Положение бонуса
  , bonusDirection :: Float  -- ^ Направление бонуса
  , bonusVelocity  :: Vector -- ^ Скорость бонуса
  , bonusSize      :: Float  -- ^ Размер бонуса
  } deriving (Generic)

instance Binary Bonus

-- | Поворот корабля
data RotateAction = ToLeft | ToRight deriving(Eq, Generic)

instance Binary RotateAction

-- | Направление ускорения корабля
data EngineAction = Forward | Back deriving(Eq, Generic)

instance Binary EngineAction

-- | Действие корабля
data ShipAction = ShipAction
  { shipID       :: PlayerID
  , rotateAction :: Maybe RotateAction
  , engineAction :: Maybe EngineAction
  , fireAction   :: Bool
  } deriving (Generic)

instance Binary ShipAction 

instance WebSocketsData ShipAction where
  fromLazyByteString = decode
  toLazyByteString   = encode

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
