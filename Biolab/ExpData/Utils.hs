module Biolab.ExpData.Utils (
    series,
    expMesTypes,
    removeDeadWells,
    liveWell,
)
where

import Biolab.Types (ExpData, Measurement(..), MType)
import Biolab.Constants (odLiveThreshold, bubbleTime)
import Biolab.Utils.Vector (Series)
import Biolab.Measurement (toPoint, mesByTime)
import qualified Data.Vector.Generic as G
import qualified Data.Map as M
import Data.List (nub)

series :: MType -> [Measurement] -> Series
series m = G.fromList . map toPoint . mesByTime m

expMesTypes :: ExpData -> [String]
expMesTypes = nub . map mType . concat . concatMap M.elems . M.elems

liveWell :: [Measurement] -> Bool -- returns whether measurements taken from a given well indicate that it grew.
liveWell ms
    | null ms = error "liveWell: empty input"
    | otherwise = odLiveThreshold < (maximum . drop bubble_length . map snd $ od_vals)
    where
        bubble_length = length . takeWhile (< fromIntegral bubbleTime) . map fst . norm_od_vals $ od_vals
        od_vals = map toPoint . mesByTime "OD600" $ ms
        norm_od_vals xs = map (\(x,y) -> (x-(fst . head $ xs), y)) $ xs

removeDeadWells :: ExpData -> ExpData
removeDeadWells = M.filter (not . M.null) . M.map (M.filter liveWell)

