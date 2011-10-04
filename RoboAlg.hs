module RoboAlg (linFitWindow, expFitWindow, maxGrowth, doublingTime, expLevel, expLevelsN)
where
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Tuple.Utils (fst3,snd3)
import Fitting

type Seconds = Integer
twoHoursWindow = 2 * 3600 + 60 -- to make sure the window includes the last measurement

toPoints :: [(Double,Seconds)] -> [Point]
toPoints = map (\(val,secs) -> (fromIntegral secs,val))

maxGrowth :: [(Double, Seconds)] -> Double
maxGrowth ms = maximum $ 0.00001 : (map fdAlpha . filter (\fd -> fdRSqr fd > 0.95) . expFitWindow twoHoursWindow . toPoints $ ms)

expLevel :: [(Double,Seconds)] -> [(Double,Seconds)]-> Double
expLevel ods ms = maximum (1 : (catMaybes $ zipWith exp_level ms_fit ods_fit))
    where
        ods_fit = expFitWindow twoHoursWindow . toPoints $ ods
        ms_fit = expFitWindow twoHoursWindow . toPoints $ ms
        exp_level f1 f2 = if fdRSqr f1 < 0.9 || fdRSqr f2 < 0.9 then Nothing else Just (fdBeta f1 - fdBeta f2)

expLevelsN :: [(Double,Seconds)] -> [(Double,Seconds)]-> [Double]
expLevelsN ods ms = zipWith ((-) `on` fdBeta) ms_fit ods_fit
    where
        ods_fit = expFitWindow twoHoursWindow . toPoints $ ods
        ms_fit = expFitWindow twoHoursWindow . toPoints $ ms

doublingTime :: [(Double, Seconds)] -> Double
doublingTime = (/) 1 . maxGrowth
