module Biolab.ExpData.AutoFluorescense (
    subtractAutoFluorescense,
)
where

import Biolab.Utils.Vector (Point, Series)
import Biolab.ExpData.Utils (series, removeDeadWells, expMesTypes)
import Biolab.Types (ExpData, MType, Measurement(..), ldMap)
import Biolab.Constants (wildTypeId, minValMap)
import Biolab.Patches (mapFst)
import Data.List (sortBy, nub, partition)
import Data.Function (on)
import qualified Data.Vector.Generic as G
import qualified Data.Map as M
import Data.Map ((!), Map)

normDistances :: [Point] -> [Point]
normDistances pts = map (mapFst (\x -> 1- (x/max_dist))) pts
    where max_dist = maximum . map fst $ pts

weightedAvg :: [Point] -> Double
weightedAvg pts = (sum . map (\(x,y) -> x*y) $ pts) / (sum . map fst $ pts)

autoFluorescense :: ExpData -> MType -> Series
autoFluorescense ed m = G.fromList . sortBy (compare `on` fst) . concatMap (auto_fl_vals) $ wt_mes
    where
        wt_mes = M.elems $ (removeDeadWells ed) ! wildTypeId
        auto_fl_vals mes = (zipWith ((,) `on` snd) `on` G.toList) (series "OD600" mes) (series m mes)

windowSize = 5

calcAF :: Series -> Double -> Double
calcAF f x = weightedAvg . normDistances . map (mapFst (abs . (x -) )) $ relevant_points
    where
        (smaller,eq_larger) = span ((x >) . fst) . G.toList $ f
        relevant_points = (take (windowSize `div` 2) . reverse $ smaller) ++ take ((windowSize `div` 2) + 1) eq_larger

autoFluorescenseMap :: ExpData -> Map MType Series
autoFluorescenseMap ed = M.fromList . zip mes_types . map (autoFluorescense ed) $ mes_types
    where
        mes_types = filter ("OD600" /=) . expMesTypes $ ed

correctAF :: Series -> [Measurement] -> [Measurement] -> [Measurement]
correctAF _ [] [] = []
correctAF afs (od:ods) (m:mes) = m {mVal = corrected_mval} : correctAF afs ods mes
        where
            corrected_mval = max (minValMap ! mt) (mVal m - (calcAF afs (mVal od)))
            mt = mType m

subtractSampleAutoFl :: Map MType Series -> [Measurement] -> [Measurement]
subtractSampleAutoFl aflm mes = ods ++ concatMap (\mt -> correctAF (aflm ! mt) od_vals (mes_vals mt)) mes_types
    where
        (ods,all_mes) = partition (("OD600" ==) . mType) mes
        mes_vals m =  time_sort . filter ((m ==) . mType) $ all_mes
        time_sort = sortBy (compare `on` mTime)
        od_vals = time_sort ods
        mes_types = nub . map mType $ all_mes

subtractAutoFluorescense :: ExpData -> ExpData
subtractAutoFluorescense ed 
    | Nothing == M.lookup wildTypeId ed = ed
    | otherwise = ldMap (subtractSampleAutoFl (autoFluorescenseMap ed)) ed
