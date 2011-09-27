module RoboAlg (linFit, expFit, maxGrowth, doublingTime)
where
import RoboStat (linearRegressionRSqr)
import Data.List (tails)
import qualified Data.Vector.Unboxed as U
import Data.Function (on)

type Seconds = Integer
twoHoursWindow = 2 * 3600 + 60 -- to make sure the window includes the last measurement

linFit :: Seconds -> [(Double,Double)] -> [(Double,Double)]
linFit sec_wind xys = map ((\(x,y,z) -> (y,z)) . linReg) $ samples
    where
        take_range secs xys = takeWhile (\x -> fst x - (fst . head $ xys) < secs) $ xys
        samples = map (take_range . fromIntegral $ sec_wind) . tails $ xys
        linReg = uncurry (linearRegressionRSqr `on` U.fromList) . unzip

expFit :: Seconds -> [(Double,Seconds)] -> [(Double,Double)]
expFit s = linFit s . map (\(x,y) -> (fromIntegral y,logBase 2 x))

maxGrowth :: [(Double, Seconds)] -> Double
maxGrowth ms = maximum $ 0.00001 : (map fst . filter (\(_,y) -> y > 0.95) . expFit twoHoursWindow $ ms)

doublingTime :: [(Double, Seconds)] -> Double
doublingTime = (/) 1 . maxGrowth
