{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, TypeSynonymInstances, OverlappingInstances #-}
import Biolab.Processing
import Biolab.Utils.Vector
import Biolab.Constants
import Data.Map ((!))
import qualified Data.Vector.Unboxed as U

main = do
    putStrLn "sanity checking - what is the maximal growth rate of a sample that is sometimes constant, sometimes exponential"
    let times = [0,1800..1800*50]
    let heading = replicate 10 0
    let growth = zipWith (**) (replicate 20 2) [0..20]
    let tail = replicate 20 (2 ** 20)
    let values = heading ++ growth ++ tail
    let test1od = U.fromList . zip times $ values
    putStrLn $ "times are:" ++ (show times)
    putStrLn $ "values are:" ++ (show values)
    putStrLn $ "calculated growth is:" ++ (show . maxGrowthRate $ test1od)
    putStrLn "sanity checking - create a vector with double the values of the od, with maturation time offset, and check the expression level"
    let fl_times = map (+ (fromIntegral maturationTime)) times
    let fl_values = map (*5) values
    putStrLn $ "fl times are:" ++ (show fl_times)
    putStrLn $ "fl values are:" ++ (show fl_values)
    let test1fl = U.fromList . zip  fl_times $ fl_values
    putStrLn $ "od fits are:" ++ (show . expFitWindow exponentialPhaseGrowthRateWindow $ test1od)
    putStrLn $ "fl fits are:" ++ (show . expFitWindow exponentialPhaseGrowthRateWindow $ test1fl)
    putStrLn $ "calculated expression level is:" ++ (show . expressionLevelEstimate maturationTime test1od $ test1fl)
    return ()
    {-
    ed <- loadExpDataDB "2011-10-30 17:07:56" 0
    let f2 = (ed ! "YM lig") ! ColonyId {cExp = "2011-10-30 17:07:56", cPlate = 0, cWell = Well {wRow = 'f', wColumn = 2}}
    let mcl = map (\x -> (fromIntegral . toSeconds . mTime $ x, mVal x)) . sortBy (compare `on` mTime) . filter (("MCHERRY" ==) . mType) $ f2
    let odl = map (\x -> (fromIntegral . toSeconds . mTime $ x, mVal x)) . sortBy (compare `on` mTime) . filter (("OD600" ==) . mType) $ f2
    let odv = U.fromList . mapFst fromIntegral $ odl
    let mcv = U.fromList . map (mapFst fromIntegral) $ mcl
    let real_time_fs = realTime 1800 mcv
    findRange (2*3600) odv real_time_fs
    expressionLevelEstimate 1800 odv real_time_fs
    let fit_data = map snd . subRange range . stdFits
    let odf = fit_data odv
    let mcf = fit_data real_time_fs
    -}
