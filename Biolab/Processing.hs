{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Biolab.Processing (
    -- Biology related functions:
    minDoublingTimeMinutes,
    doublingTimeMinutes,
    expressionLevelEstimate,
    expressionLevels,
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
doublingTimeMinutes :: Series -> Series
doublingTimeMinutes = G.fromList . filter ((300>) . snd) . filter ((10<) . snd) . map (mapSnd ((/60) . (1/) .  fdAlpha)) . filter ((0.90 <)  . fdRSqr . snd) . expFitWindow exponentialPhaseGrowthRateWindow

minDoublingTimeMinutes :: Series -> Double
minDoublingTimeMinutes s
    | G.null (doublingTimeMinutes s) = 0
    | otherwise = minimum . drop 2 . map snd . G.toList . doublingTimeMinutes $ s

realTime :: Seconds -> Series -> Series
realTime s = vxMap (-(fromIntegral s) +)

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
type ExpressionLevelEstimateAtConstOd = Double -> Seconds -> Series -> Series -> Double

derivativeEstimation :: ExpressionLevelEstimateAtConstOd
derivativeEstimation target_od mat_time ods fs = logBase 2 $ 0.1 + (derivate real_time_fs mid_od_time) / od_val
    where
        real_time_fs = realTime mat_time fs
        (mid_od_time,od_val) = fromMaybe default_point . find ((target_od <) . snd) . G.toList $ ods
        default_point = G.head . G.drop 3 $ ods

integralEstimation :: ExpressionLevelEstimateAtConstOd
integralEstimation target_od mat_time ods fs = logBase 2 $ 0.1 + (abs $ delta real_time_fs mid_od_time) / integrate ods mid_od_time
    where
        real_time_fs = realTime mat_time fs
        mid_od_time = fst . fromMaybe default_point . find ((target_od <) . snd) . G.toList $ ods
        default_point = G.head . G.drop 3 $ ods


meanEstimation :: ExpressionLevelEstimateAtConstOd -> Seconds -> Series -> Series -> Double
meanEstimation f mat_time ods fs = mean . map (\x -> f x mat_time ods fs) $ od_vals
    where
        od_vals = take 5 . map snd . fromMaybe [] . find ((mid_od <) . snd . (!! 2)) . tails . G.toList $ ods

expressionLevelEstimate :: Seconds -> Series -> Series -> Double
expressionLevelEstimate = meanEstimation integralEstimation

-- can be plugged into expressionLevels for interrogation.
expressionLevelByOD :: ExpressionLevelEstimateAtConstOd -> Seconds -> Series -> Series -> Series
expressionLevelByOD f mat_time ods fs = G.fromList . map (\(t,od) -> (od, f od mat_time real_ods fs)) . G.toList $ real_ods
    where
        real_ods = G.fromList . dropWhile ((min_od <) . snd) . takeWhile ((max_od >) . snd) . G.toList $ ods
        min_od = minimum . map snd . G.toList $ ods
        max_od = maximum . map snd . G.toList $ ods

expressionLevels :: Seconds -> Series -> Series -> Series
expressionLevels = expressionLevelByOD integralEstimation
