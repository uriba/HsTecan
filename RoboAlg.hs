module RoboAlg (linFitWindow, expFitWindow, maxGrowth, doublingTime, expLevelEst, expLevelsN,expLevelsF)
where
import Data.Function (on)
import Data.List (sortBy,tails,elemIndex)
import Data.Maybe (catMaybes,fromMaybe)
import Data.Tuple.Utils (fst3,snd3,thd3)
import Fitting
import Math.Statistics

type Seconds = Integer
twoHoursWindow = 2 * 3600 + 60 -- to make sure the window includes the last measurement

toPoints :: [(Double,Seconds)] -> [Point]
toPoints = map (\(val,secs) -> (fromIntegral secs,val))

maxGrowth :: [(Double, Seconds)] -> Double
maxGrowth ms = maximum $ 0.00001 : (map fdAlpha . filter (\fd -> fdRSqr fd > 0.95) . expFitWindow twoHoursWindow . toPoints $ ms)

expLevelEst :: [(Double,Seconds)] -> [(Double,Seconds)]-> Double
expLevelEst ods ms = thd3 . head . sortBy (compare `on` fst3) . filter (\(x,y,z) -> not . isNaN $ x*y*z) $ els
-- snd . head . sortBy (compare `on` fst) . filter (not . isNaN . snd) $ (catMaybes $ zipWith exp_level ms_fit ods_fit)
    where
        ods_fit = expFitWindow twoHoursWindow . toPoints $ ods
        ms_fit = expFitWindow twoHoursWindow . toPoints $ ms
        els = map (\(a,b,c,d) -> (b,c,d)) . expLevels' ods_fit $ ms_fit
        -- exp_level od fl = if fdRSqr od < 0.9 || fdRSqr fl < 0.9 then Nothing else Just (fdAlpha od,snd . expLevel od $ fl)

alphas :: [FitData] -> [Double]
alphas = map fdAlpha

expLevelsF :: [Point] -> [Point]-> [(Double,Double)]
expLevelsF ods mss = map (\(a,b,c,d) -> (a,d)) . expLevels' ods_fit $ ms_fit
    where
        ods_fit = expFitWindow twoHoursWindow ods
        ms_fit = expFitWindow twoHoursWindow mss

expLevels' :: [FitData] -> [FitData] -> [(Double,Double,Double,Double)]
expLevels' _ [] = []
expLevels' [] _ = []
expLevels' odf flf = (st,gr,rs,el) : expLevels' (tail odf) (drop os flf)
    where
        st = fdStart . head $ odf
        gr = fdAlpha . head $ odf
        (rs,el) = expLevel (head odf) . head . drop os $ flf
        os = bestCorrelationOffset 3 (alphas odf) . tails . alphas $ flf

bestCorrelationOffset :: Int -> [Double] -> [[Double]] -> Int
bestCorrelationOffset window s1 s2s = fromMaybe 0 . elemIndex (minimum diffs) $ diffs
    where
        diffs = map (\s2 -> zipWith (\x1 x2 -> (x1 - x2) ** 2) (take window s1) s2) relevant_tails
        relevant_tails = takeWhile (\s -> length s >= window) s2s
        
expLevel :: FitData -> FitData -> (Double,Double)
expLevel od fl = ((fdAlpha od - fdAlpha fl) ** 2,fdAlpha od + fdBeta fl - fdBeta od)

expLevelsN :: [(Double,Seconds)] -> [(Double,Seconds)]-> [Double]
expLevelsN ods ms = zipWith ((-) `on` fdBeta) ms_fit ods_fit
    where
        ods_fit = expFitWindow twoHoursWindow . toPoints $ ods
        ms_fit = expFitWindow twoHoursWindow . toPoints $ ms

doublingTime :: [(Double, Seconds)] -> Double
doublingTime = (/) 1 . maxGrowth
