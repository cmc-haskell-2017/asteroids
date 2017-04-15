module Config where


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
