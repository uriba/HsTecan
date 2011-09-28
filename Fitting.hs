module Fitting (Point,linFitWindow,expFitWindow)
where
import qualified Data.Vector.Unboxed as U
import RoboStat (linearRegressionRSqr)

tails :: (U.Unbox a) => U.Vector a -> [U.Vector a]
tails v
    | U.null v = [U.empty]
    | otherwise = v : (tails . U.tail $ v)

type Point = (Double,Double)
linFit :: U.Vector Point -> (Double,Double,Double)
linFit = uncurry linearRegressionRSqr . U.unzip

takeXRange :: Double -> U.Vector Point -> U.Vector Point
takeXRange range pts = U.takeWhile in_range pts
    where
        in_range pt = fst pt - start_val < range
        start_val = fst . U.head $ pts

linFitWindow :: Double -> U.Vector Point -> [(Double,Double,Double)]
linFitWindow sec_wind pts = map linFit windows
    where
        windows = map (takeXRange sec_wind) . tails $ pts

toLog :: U.Vector Point -> U.Vector Point
toLog = U.map (\(x,y) -> (x,logBase 2 y))

expFit :: U.Vector Point -> (Double,Double,Double)
expFit = linFit . toLog

expFitWindow :: Double -> U.Vector Point -> [(Double,Double,Double)]
expFitWindow w = linFitWindow w . toLog
