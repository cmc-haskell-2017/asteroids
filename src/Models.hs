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
  , imageB_oil      :: Picture -- ^ Бонус-топливо
  , imageB_low      :: Picture -- ^ Бонус-замедление
  , imageB_high     :: Picture -- ^ Бонус-ускорение
  , imageB_gun      :: Picture -- ^ Бонус-оружие
  , imageB_def      :: Picture -- ^ Бонус-защита
  , imageStat       :: Picture -- ^ Фон статистики
  , imageShield     :: Picture -- ^ Защитный экран
  , imageB_clear    :: Picture -- ^ Бонус-очищение экрана от астероидов
  , imageB_barrier  :: Picture -- ^ Торможение бонусов
  }

-- | Статистика
data Table = Table 
  { tablePosition :: Point -- ^ Положение фона статистики
  } deriving (Generic)

instance Binary Table

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
  , shieldTime          :: Float      -- ^ Время действия защитного экрана
  , flag                :: Bool       -- ^ Зачистка экрана
  } deriving (Generic)

instance Binary Spaceship

-- | Пуля
data Bullet = Bullet
  { bulletID        :: PlayerID -- ^ Какому кораблю принадлежит пуля  
  , bulletPosition  :: Point  -- ^ Положение пули
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
  , tableback      :: Maybe Table -- ^ Фон статистики
  , table          :: Maybe Table -- ^ Сама статистика
  , freshPositions :: [Point]     -- ^ Бесконечный список "свежих" позиций для кораблей
  , freshAsteroids :: [Asteroid]  -- ^ Бесконечный список "свежих" астероидов
  , freshBonuses   :: [Bonus]     -- ^ Бесконечный список "свежих" бонусов
  , scores         :: [Score]     -- ^ Список счета каждого корабля
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
  , astBonusTime      :: (Bool, Float)  -- ^ Активен ли бонус 
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

-- | Счет
data Score = Score
  { scoreID        :: PlayerID   -- ^ Имя корабля
  , scoreMode      :: Mode
  , scoreAst       :: Int  -- ^ Количество убитых астероидов
  , scoreShip      :: Int  -- ^ Количество убитых кораблей
  , scoreDeath     :: Int  -- ^ Количество собстенных смертей
  , scoreBonus     :: Int  -- ^ Количество собранных бонусов
  } deriving (Generic)

instance Binary Score

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
