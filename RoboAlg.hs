module RoboAlg (linFitWindow, expFitWindow, maxGrowth, doublingTime, aExpLevel)
where
import Data.Function (on)
import Data.List (sortBy)
import Data.Tuple.Utils (fst3,snd3)
import Fitting
import qualified Data.Vector.Unboxed as U

type Seconds = Integer
twoHoursWindow = 2 * 3600 + 60 -- to make sure the window includes the last measurement

toPoints :: [(Double,Seconds)] -> U.Vector Point
toPoints = U.fromList . map (\(val,secs) -> (fromIntegral secs,val))

maxGrowth :: [(Double, Seconds)] -> Double
maxGrowth ms = maximum $ 0.00001 : (map snd3 . filter (\(_,_,r) -> r > 0.95) . expFitWindow twoHoursWindow . toPoints $ ms)

aExpLevel :: [(Double,Seconds)] -> Double
aExpLevel ms = head . map fst3 . sortBy (compare `on` snd3) $ (0.000001, 0.0000001,1) : (filter (\(_,_,r) -> r > 0.95) . expFitWindow twoHoursWindow . toPoints $ ms)

doublingTime :: [(Double, Seconds)] -> Double
doublingTime = (/) 1 . maxGrowth
