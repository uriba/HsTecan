module RoboAlg (
    linFitWindow,
    expFitWindow,
    maxGrowth,
    doublingTime,
    expLevelEst,
    expLevelsN,
    expLevelsBestGRCorrelation
)
where
import Data.Function (on)
import Data.List (maximumBy, sortBy, tails, elemIndex)
import Data.Maybe (catMaybes,fromMaybe,fromJust)
import Data.Tuple.Utils (fst3,snd3,thd3)
import Fitting
import Biolab.Patches (mean)
import qualified Data.Vector.Unboxed as U

type Seconds = Integer

twoHoursWindow = 2 * 3600 + 60 -- to make sure the window includes the last measurement

toPoints :: [(Double,Seconds)] -> SampleData
toPoints = U.fromList . map (\(val,secs) -> (fromIntegral secs,val))

maxGrowth :: [(Double, Seconds)] -> Double
maxGrowth ms = maximum $ 0.00001 : (map fdAlpha . filter (\fd -> fdRSqr fd > 0.95) . expFitWindow twoHoursWindow . toPoints $ ms)

-- Alternatives are:
-- take the maximal growth rate and use it's calculated expression level.
-- pick a window with highest growth rate of OD/Fl and calculate the expression level on these windows.
expLevelEst :: [(Double,Seconds)] -> [(Double,Seconds)]-> Double
expLevelEst ods ms =  mean . map snd . take 3 . drop 2 . reverse . sortBy (compare `on` fst) . expLevelsBestGRCorrelation od_pts $ ms_pts
    where
        od_pts = toPoints ods
        ms_pts = toPoints ms
        ods_fit = expFitWindow twoHoursWindow od_pts
        ms_fit = expFitWindow twoHoursWindow ms_pts

alphas :: [FitData] -> [Double]
alphas = map fdAlpha

expLevelsBestGRCorrelation :: SampleData -> SampleData -> [(Double,Double)]
expLevelsBestGRCorrelation ods mss = map (\(a,b,c,d) -> (a,d)) . expLevelsBestGRCorrelation' ods_fit $ ms_fit
    where
        ods_fit = expFitWindow twoHoursWindow ods
        ms_fit = expFitWindow twoHoursWindow mss

expLevelsBestGRCorrelation' :: [FitData] -> [FitData] -> [(Double,Double,Double,Double)]
expLevelsBestGRCorrelation' _ [] = []
expLevelsBestGRCorrelation' [] _ = []
expLevelsBestGRCorrelation' odf flf = (st,gr,rs,el) : expLevelsBestGRCorrelation' (tail odf) (drop os flf)
    where
        st = fdStart . head $ odf
        gr = fdAlpha . head $ odf
        (rs,el) = expLevel (head odf) . head . drop os $ flf
        os = bestCorrelationOffset 3 (alphas odf) . tails . alphas $ flf

bestCorrelationOffset :: Int -> [Double] -> [[Double]] -> Int
bestCorrelationOffset window s1 s2s = fromMaybe 0 . elemIndex (minimum diffs) $ diffs
    where
        diffs = map (sum . (\s2 -> zipWith (\x1 x2 -> (x1 - x2) ** 2) (take window s1) s2)) relevant_tails
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

highestGrowthRateWindow :: Int -> [FitData] -> [FitData]
highestGrowthRateWindow window fds = maximumBy (compare `on` (mean . alphas)) $ fd_windows
    where
        fd_windows = filter ((window ==) . length) . map (take window) . tails $ fds
