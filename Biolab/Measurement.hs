module Biolab.Measurement (
    filterBy,
    filterByWell,
    filterByWells,
    filterByType,
    valByTime,
    mesByTime,
    wellFromInts,
    toPoint,
)
where

import Biolab.Types
import Biolab.Utils.Vector (Point)
import Biolab.Constants
import Data.List (sortBy, nub)
import Data.Function (on)
import Data.DateTime (toSeconds)
import qualified Data.ListLike as LL

filterBy :: (Eq b, LL.ListLike full a) => (a -> b) -> b -> full -> full
filterBy f v = LL.filter ((==) v . f)

filterByWell :: Well -> [Measurement] -> [Measurement]
filterByWell = filterBy mWell

filterByType :: String -> [Measurement] -> [Measurement]
filterByType = filterBy mType

filterByWells :: [Well] -> [Measurement] -> [Measurement]
filterByWells ws = concat . zipWith filterByWell ws . repeat

valByTime :: MType -> [Measurement] -> [Double]
valByTime mt = map mVal . mesByTime mt

toPoint :: Measurement -> Point
toPoint x = (fromIntegral . toSeconds . mTime $ x, mVal x)

mesByTime :: MType -> [Measurement] -> [Measurement]
mesByTime mt = sortBy (compare `on` mTime) . filterByType mt . verifySingleColony

verifySingleColony :: [Measurement] -> [Measurement]
verifySingleColony ms
    | null ms = error "empty list of measurements given"
    | 1 < (length . nub . map colonyId $ ms) = error $ "measurements for more than one colony given" ++ show ms
    | otherwise = ms

wellFromInts :: Int -> Int -> Well
wellFromInts r c = Well { wRow = ['a'..'h'] !! r, wColumn = c + 1 }
