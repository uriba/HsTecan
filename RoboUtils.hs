module RoboUtils ( --rename to RoboCommon
    filterBy,
    maxMes,
)
where

maxMes :: Double
maxMes = 70000

filterBy :: (Eq b) => (a -> b) -> b -> [a] -> [a]
filterBy f v = filter ((==) v . f)
