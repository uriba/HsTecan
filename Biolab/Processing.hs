{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Biolab.Processing (
    -- Biology related functions:
    minDoublingTimeMinutes,
    doublingTimeMinutes,
    doublingTimeMinutesPerOD,
    expressionLevelEstimate,
    expressionLevels,
    yield,
)
where

import Statistics.LinearRegression (linearRegressionRSqr)
import qualified Data.Vector.Generic as G
import Biolab.Utils.Vector
import Biolab.Types
import Biolab.Patches (mean, isLegal, mapFst, mapSnd)
import Biolab.Constants
import Data.Maybe (fromJust, catMaybes, isJust, fromMaybe)
import Data.List (tails, sort, find, maximumBy, genericDrop, zipWith4)
import Data.Function (on)

-- this file should contain Biology specific functionality.
exponentialPhaseGrowthRateWindowMinutes = exponentialPhaseGrowthRateWindow / 60

-- estimate max growth rate based on steepest slope of linear fit in log space of measurements given, running on a predefined length window.
filterIrrelevantTimes :: [Point] -> Series
filterIrrelevantTimes = G.fromList . filter ((600>) . snd) . filter ((10<) . snd)

fitsData :: Series -> [(Double, FitData)]
fitsData = filter ((0.90 <)  . fdRSqr . snd) . expFitWindow exponentialPhaseGrowthRateWindow

doublingTimeMinutes :: Series -> Series
doublingTimeMinutes =  filterIrrelevantTimes . map (mapSnd ((/60) . (1/) .  fdAlpha)) . fitsData

doublingTimeMinutesPerOD :: Series -> Series
doublingTimeMinutesPerOD = filterIrrelevantTimes . map (\(_,fd) -> (2 ** (fdBeta fd), 1/(60*fdAlpha fd))) . fitsData

minDoublingTimeMinutes :: Series -> Double
minDoublingTimeMinutes s
    | G.null (doublingTimeMinutes s) = 0
    | otherwise = minimum . map (mean . take 3) . filter ((3>=) . length) . tails . map snd . G.toList . doublingTimeMinutes $ s

realTime :: Series -> Series
realTime = vxMap (-(fromIntegral maturationTime) +)

-- helper functions - should probably move elsewhere
windowOfSize :: Int -> Double -> Series -> Series
windowOfSize ws x xs = G.take ws . G.drop data_before_x $ xs
    where
        data_before_x = (length . takeWhile ((x>) . fst) . G.toList $ xs) - (ws `div` 2)

derivate :: Series -> Double -> Double
derivate f x = a
    where
        a = fdAlpha . snd . fitData . windowOfSize 5 x $ f

integrate :: Series -> Double -> Double
integrate f x = sum . map snd . G.toList $ windowOfSize window_size x f

delta :: Series -> Double -> Double
delta f x = let w = windowOfSize window_size x f
            in (snd . G.last $ w) - (snd . G.head $ w)
        

mid_od = 0.1
window_size = 5

---------------------------------------------------
type ExpressionLevelEstimateAtConstOd = Double -> Series -> Series -> Double

derivativeEstimation :: ExpressionLevelEstimateAtConstOd
derivativeEstimation target_od ods fs = logBase 2 $ 0.1 + (derivate (realTime fs) mid_od_time) / od_val
    where
        (mid_od_time,od_val) = fromMaybe default_point . find ((target_od <) . snd) . G.toList $ ods
        default_point = G.head . G.drop 3 . check $ ods

check :: Series -> Series
check v = if G.length v < 4 then error "too short" else v

integralEstimation :: ExpressionLevelEstimateAtConstOd
integralEstimation target_od ods fs = logBase 2 $ 0.1 + (abs $ delta (realTime fs) mid_od_time) / integrate ods mid_od_time
    where
        mid_od_time = fst . fromMaybe default_point . find ((target_od <) . snd) . G.toList $ ods
        default_point = G.head . G.drop 3 . check $ ods


meanEstimation :: ExpressionLevelEstimateAtConstOd -> Series -> Series -> Double
meanEstimation f ods fs = mean . map (\x -> f x ods fs) $ od_vals
    where
        od_vals = take 5 . map snd . fromMaybe [] . find ((mid_od <) . snd . (!! 2)) . tails . G.toList $ ods

expressionLevelEstimate :: Series -> Series -> Double
expressionLevelEstimate = meanEstimation integralEstimation

-- can be plugged into expressionLevels for interrogation.
expressionLevelByOD :: ExpressionLevelEstimateAtConstOd -> Series -> Series -> Series
expressionLevelByOD f ods fs = G.fromList . map (\(t,od) -> (od, f od real_ods fs)) . G.toList $ real_ods
    where
        real_ods = G.fromList . dropWhile ((min_od <) . snd) . takeWhile ((max_od >) . snd) . G.toList $ ods
        min_od = minimum . map snd . G.toList $ ods
        max_od = maximum . map snd . G.toList $ ods

expressionLevels :: Series -> Series -> Series
expressionLevels = expressionLevelByOD integralEstimation

yield :: Series -> Double
yield = mean . take 3 . drop 2 . reverse . sort . map snd . G.toList
