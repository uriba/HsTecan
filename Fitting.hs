module Fitting (Point,linFitWindow,expFitWindow,FitData(..))
where
import Math.Statistics (linreg)
import Data.List (tails)

data FitData = FitData { fdAlpha :: Double, fdBeta :: Double, fdRSqr :: Double, fdStart :: Double }
type Point = (Double,Double)

takeXRange :: Double -> [Point] -> [Point]
takeXRange range pts = takeWhile in_range pts
    where
        in_range pt = fst pt - start_val < range
        start_val = fst . head $ pts

fitData :: [Point] -> FitData
fitData pts = FitData { fdAlpha = a, fdBeta = b, fdRSqr = r*r, fdStart = start }
    where
        (b,a,r) = linreg . map (\(x,y) -> (x-start,y)) $ pts -- normialize points to start from 0.
        start = fst . head $ pts

linFitWindow :: Double -> [Point] -> [FitData]
linFitWindow sec_wind pts = map fitData windows
    where
        windows = map (takeXRange sec_wind) . tails $ pts

toLog :: [Point] -> [Point]
toLog = map (\(x,y) -> (x,logBase 2 y))

expFitWindow :: Double -> [Point] -> [FitData]
expFitWindow w = linFitWindow w . toLog
