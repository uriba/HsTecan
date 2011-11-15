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
import Data.Maybe (fromJust)
import Data.List (sort, find, maximumBy, genericDrop)
import Data.Function (on)

-- this file should contain Biology specific functionality.
exponentialPhaseGrowthRateWindowMinutes = exponentialPhaseGrowthRateWindow / 60
-- estimate max growth rate based on steepest slope of linear fit in log space of the OD, running on a predefined length window.
doublingTimeMinutes :: Series -> Series
doublingTimeMinutes = G.fromList . filter ((300>) . snd) . filter ((10<) . snd) . map (mapSnd ((/60) . (1/) .  fdAlpha)) . filter ((0.90 <)  . fdRSqr . snd) . expFitWindow exponentialPhaseGrowthRateWindow

minDoublingTimeMinutes :: Series -> Double
minDoublingTimeMinutes s
    | G.null (doublingTimeMinutes s) = 0
    | otherwise = minimum . drop 2 . map snd . G.toList . doublingTimeMinutes $ s
-- Alternatives are:
-- take the maximal growth rate and use it's calculated expression level.
-- pick a window with highest growth rate of OD/Fl and calculate the expression level on these windows.

realTime :: Seconds -> Series -> Series
realTime s = vxMap (-(fromIntegral s) +)

expressionLevelEstimate :: Seconds -> Series -> Series -> Double
expressionLevelEstimate mat_time ods fs =  mean . map snd . filter (isLegal . snd) . zipWith expressionLevel (fit_data ods) $ (fit_data real_time_fs)
    where
        range = findRange (floor exponentialPhaseGrowthRateWindow) ods real_time_fs
        real_time_fs = realTime mat_time fs
        fit_data = map snd . subRange range . stdFits

expressionLevels :: Seconds -> Series -> Series -> Series
expressionLevels mat_time ods fs = G.fromList $ zipWith timed_el (stdFits ods) (stdFits real_time_fs)
    where
        real_time_fs = realTime mat_time fs
        timed_el (t1,odf) (t2,flfd) = (t1,snd $ expressionLevel odf flfd) -- log/verify times?

stdFits = expFitWindow exponentialPhaseGrowthRateWindow

expressionLevel :: FitData -> FitData -> (Double,Double)
expressionLevel od fl = ((fdAlpha od - fdAlpha fl) ** 2,fdAlpha od + fdBeta fl - fdBeta od)

-- return a time range of length r containing the maximal measurements of the two series.
findRange :: Seconds -> Series -> Series -> (Double,Double)
findRange r s1 s2 = fromJust . find ((rng <=) . range) . map (overlapping_range s1 s2) $ thresholds s1 s2
    where
        interval s = floor $ (fst . G.head . G.tail $ s) - (fst . G.head $ s)
        thresholds s1 s2 = genericDrop (2 * r `div` interval s1) . reverse . sort . filter isLegal . map snd $ (G.toList s1 ++ G.toList s2)
        overlapping_range s1 s2 t = maximumBy (compare `on` range) $ (0,0) : intersectingRanges (ranges (> t) s1) (ranges (> t) s2)
        rng = fromIntegral r
