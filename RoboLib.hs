module RoboLib (
    mesData,
    timedMesData,
    mesToOdData,
    timedMesToOdData,
    intensityGridData,
    PlotLinesData,
    TimedPlotLinesData,
    PlotGridData,
    expLevel',
    noTrans,
    wellStr,
    AxesTrans,
    )
where
import Data.Function (on)
import Data.List
import Data.Map ((!), Map(..), fromList)
import Data.Maybe
import Data.DateTime (DateTime, toSeconds)
import RoboAlg
import qualified Data.Map as M
import Biolab.Constants
import Biolab.Types
import Biolab.Measurement (mesByTime, valByTime, filterBy)
import Biolab.ExpData
import Biolab.Patches (mean, isLegal)

-- Some types to make life easier...
type ExpLevelData = LabeledData [(MType,[Double])] -- expression levels for each colony and measurement type.
type PlotLinesData = LabeledData [Double] -- for each label - a list of colonies, for each colony - a line.
type TimedPlotLinesData = LabeledData [(Double, DateTime)] -- for each label - a list of colonies, for each colony - a line.
type PlotGridData = LabeledData (Double,Double) -- for each label - a list of colonies, for each colony - a line.

autoFluorescenceMap :: ExpData -> Maybe MesTypeCorrectionVals
autoFluorescenceMap ed = do
    af_colonies <- M.lookup wildTypeId ed
    let af_mes = M.elems af_colonies
    let af m = mean . map (expLevel' m) $ af_mes
    return . M.fromList $ [(m, af m) | m <- expMesTypes ed]

timedMesData :: ExpData -> MType -> TimedPlotLinesData
timedMesData ed mt = ldMap (map (\x -> (mVal x, mTime x)) . mesByTime mt) (normalizePlate ed)

mesData :: ExpData -> MType -> PlotLinesData
mesData ed mt = ldMap (map fst) . timedMesData ed $ mt

mesToOd :: MType -> Maybe (Int,Int) -> [Measurement] -> [Double]
mesToOd mt m_range ms = map fst . timedMesToOd mt m_range $ ms


timedMesToOd :: MType -> Maybe (Int,Int) -> [Measurement] -> [(Double,DateTime)]
timedMesToOd mt Nothing ms = timedMesToOd mt (Just . mesLimits mt $ ms) $ ms 
timedMesToOd mt (Just (low,high)) ms = removeIllegalPoints m_to_od_vals
    where
        mes t = take (high - low) . drop low . mesByTime t
        vals t = map mVal . mes t
        diffs x = zipWith (-) (tail x) x
        m_to_od_vals = zipWith (\x y -> (x/mVal y, mTime y)) (diffs . vals mt $ ms) . mes "OD600" $ ms

removeDeadWells :: ExpData -> ExpData
removeDeadWells = M.filter (not . M.null) . M.map (M.filter liveWell)

removeIllegalPoints = filter (isLegal . fst)

timedMesToOdData :: ExpData -> MType -> Maybe (Int, Int) -> TimedPlotLinesData
timedMesToOdData ed mt m_range = ldMap (removeIllegalPoints . map (\(x,y) -> 
                    (logBase 10 x, y)) . timedMesToOd mt m_range) nbg
    where
        nbg = removeDeadWells . normalizePlate $ ed
	
-- consider combining code with plotGridData (currently problem is requirement for specifying limits here)
mesToOdData :: ExpData -> MType -> Maybe (Int, Int) -> PlotLinesData
mesToOdData ed mt m_range = ldMap (map fst) . timedMesToOdData ed mt $ m_range
	
minIdx :: (Ord a) => [a] -> Int
minIdx xs = fromJust . findIndex ((==) . minimum $ xs) $ xs

odLimits :: [Double] -> (Int,Int) -- assumes time-wise sorted list of OD measurements.
odLimits ods = (low,high)
    where
        normalized = minimum ods == minValMap ! "OD600"
        od_threshold = if normalized then odThreshold else odThreshold + stdMinOd
        rods = reverse ods
        low = length ods - (fromMaybe (minIdx rods) . findIndex (< od_threshold) $ rods)
        high = length ods -- (fromJust . findIndex (< upper_limit ods) $ rods)
        upper_limit x = (minimum x + maximum x) / 2

mesLimits :: MType -> [Measurement] -> (Int,Int)
mesLimits mt ms = (low,high)
    where
	(low,high_od) = odLimits . valByTime "OD600" $ ms
	high = min high_od . fromMaybe (high_od) . findIndex (>= maxMes) . valByTime mt $ ms

expLevel' :: MType -> [Measurement] -> Double
expLevel' mt = mean . mesToOd mt Nothing

expLevels :: ExpData -> ExpLevelData
expLevels ed = ldMap (\x -> [(m,if m == "OD600" then repeat . maxGrowth . od_mes $ x else mesToOd m Nothing x) | m <- expMesTypes ed]) ed
    where
        od_mes = sortBy (compare `on` snd) . map (\x -> (mVal x,toSeconds . mTime $ x)) . filter (\x -> mType x == "OD600")

subtractAutoFluorescence :: ExpData -> ExpLevelData -- need adjustment for df/dt and maximum value selection
subtractAutoFluorescence ed = ldMap (map (\(mt,vals) -> (mt, map (corrected_el mt) vals))) . expLevels $ ed
    where
	corrected_el mt x = fromMaybe x $ do
	    afm <- autoFluorescenceMap ed
	    af <- M.lookup mt afm
	    return $ max (minValMap ! mt) (x - af)

type AxesTrans = ((Double -> Double),(Double -> Double))

noTrans = (id,id)

intensityGridData :: ExpData -> (String,String) -> AxesTrans -> PlotGridData
intensityGridData ed (xtype,ytype) (fx,fy) = grid_points
    where
        ned = removeDeadWells . normalizePlate $ ed
        m_mes m = sortBy (compare `on` snd) . map (\x -> (mVal x,toSeconds . mTime $ x)) . filter (\x -> mType x == m)
        exp_c m ms = expLevelEst (m_mes "OD600" ms) (m_mes m ms)
        exp_level mt ms = exp_c mt ms
        grid_points = ldMap (\x -> (exp_level xtype x,exp_level ytype $ x)) ned

growthGridData :: ExpData -> (String,String) -> AxesTrans -> PlotGridData
growthGridData ed (xtype,ytype) (fx,fy) = grid_points
    where
        data_sets = {-subtractAutoFluorescence . -}expLevels . removeDeadWells . normalizePlate $ ed
        plot_vals = ldMap (map (\(mt,vals) -> (mt, calcexp . take 100 $ vals))) data_sets
        grid_points = ldMap (\x -> (fx $ (fromJust . lookup xtype $ x) + (fromJust . lookup ytype $ x),fromJust . lookup "OD600" $ x)) plot_vals
        calcexp = mean . take 3 . drop 2 . reverse . sort

intensityGridData' :: ExpData -> (String,String) -> AxesTrans -> PlotGridData
intensityGridData' ed (xtype,ytype) (fx,fy) = grid_points
    where
        data_sets = {-subtractAutoFluorescence . -}expLevels . removeDeadWells . normalizePlate $ ed
        plot_vals = ldMap (map (\(mt,vals) -> (mt, calcexp vals))) data_sets
        grid_points = ldMap (\x -> (fx . fromJust . lookup xtype $ x,fy . fromJust . lookup ytype $ x)) plot_vals
        calcexp = mean . take 3 . drop 2 . reverse . sort

wellStr :: Well -> String
wellStr w = concat ["(", [wRow w],",",show . wColumn $ w,")"]
