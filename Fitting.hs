module Fitting (Point,SampleData,linFitWindow,expFitWindow,FitData(..))
where
import Statistics.LinearRegression (linearRegressionRSqr)
import Data.Vector.Unboxed (Vector)
import qualified Data.ListLike as LL
import Data.ListLike.Vector.Unboxed

data FitData = FitData { fdAlpha :: Double, fdBeta :: Double, fdRSqr :: Double, fdStart :: Double }
    deriving Show

type Point = (Double,Double)
type SampleData = Vector Point

takeXRange :: Double -> SampleData -> SampleData
takeXRange range pts = LL.takeWhile in_range pts
    where
        in_range pt = fst pt - start_val < range
        start_val = fst . LL.head $ pts

fitData :: SampleData -> FitData
fitData pts = FitData { fdAlpha = a, fdBeta = b, fdRSqr = r*r, fdStart = start }
    where
        (b,a,r) = linearRegressionRSqr xs ys
        xs = LL.map (\(x,_) -> x-start) pts -- normialize points to start from 0.
        ys = LL.map (\(_,y) -> y) pts
        start = fst . LL.head $ pts

linFitWindow :: Double -> SampleData -> [FitData]
linFitWindow sec_wind pts = map fitData windows
    where
        windows = map (takeXRange sec_wind) . LL.tails $ pts

toLog :: SampleData -> SampleData
toLog = LL.map (\(x,y) -> (x,logBase 2 y))

expFitWindow :: Double -> SampleData -> [FitData]
expFitWindow w = filter (\x -> not . isNaN . fdAlpha $ x) . linFitWindow w . toLog
