module RoboLib (
    Well (..),
    ColonyId (..),
    colonyId,
    wellFromInts,
    Measurement (..),
    createExpData,
    mesData,
    timedMesData,
    mesToOdData,
    timedMesToOdData,
    intensityGridData,
    plotGridDataToStrings,
    plotLinesDataToStrings,
    PlotLinesData,
    TimedPlotLinesData,
    PlotGridData,
    ExpData,
    ExpId,
    MType,
    expLevel',
    noTrans,
    expMesTypes,
    smoothAll,
    bFiltS,
    has,
    Label,
    wellStr,
    AxesTrans,
    normalizePlate
    )
where
import Data.Function (on)
import Data.List
import Data.Map ((!), Map(..), fromList)
import Data.Maybe
import Data.DateTime (DateTime, toSeconds)
import RoboAlg
import RoboUtils
import qualified Statistics.KernelDensity as KD
import qualified Data.Map as M

-- Some types to make life easier...
type Label = String
type MType = String
type ExpId = String
type PlateId = Int
data Well = Well { wRow :: Char , wColumn :: Int } deriving (Eq, Show, Read, Ord)
data ColonyId = ColonyId { cExp :: ExpId, cPlate :: Int, cWell :: Well } deriving (Eq, Show, Ord)

data Measurement = Measurement { mExpDesc :: ExpId, mPlate :: PlateId, mTime :: DateTime, mType :: MType, mWell :: Well, mLabel :: Label, mVal :: Double } deriving (Eq, Show)

colonyId :: Measurement -> ColonyId
colonyId (Measurement { mExpDesc = ed, mPlate = mp, mWell = mw }) = ColonyId { cExp = ed, cPlate = mp, cWell = mw }

wildTypeId = "WT" -- label of colonies that have wt bacteria (for auto-fluorescence cancellation)
mediaId = "BLANK"  -- label of colonies that have no bacteria (for background cancellation)

type ExpData = Map Label (Map ColonyId [Measurement]) -- experiment data is mapped like this.

type ExpLevelData = Map Label (Map ColonyId [(MType,[Double])]) -- expression levels for each colony and measurement type.

type PlotLinesData = Map Label (Map ColonyId [Double]) -- for each label - a list of colonies, for each colony - a line.
type TimedPlotLinesData = Map Label (Map ColonyId [(Double, DateTime)]) -- for each label - a list of colonies, for each colony - a line.
type PlotGridData = Map Label (Map ColonyId (Double,Double)) -- for each label - a list of colonies, for each colony - a line.

-- utils for outputting plot data to files
linesData :: (String,Map ColonyId [Double]) -> [[String]]
linesData (label,lines) = map (lineData label) . M.toList $ lines

lineData :: String -> (ColonyId,[Double]) -> [String]
lineData label (cid,points) = [
	label,
	cExp cid,
	show . cPlate $ cid,
	[wRow . cWell $ cid],
	show . wColumn . cWell $ cid
    ] ++ map show points

plotLinesDataToStrings :: PlotLinesData -> [[String]]
plotLinesDataToStrings = concatMap linesData . M.toList

pointsData :: (String,Map ColonyId (Double,Double)) -> [[String]]
pointsData (label,points) = map (pointData label) . M.toList $ points

pointData :: String -> (ColonyId,(Double,Double)) -> [String]
pointData label (cid,(x,y)) = [
	label,
	cExp cid,
	show . cPlate $ cid,
	[wRow . cWell $ cid],
	show . wColumn . cWell $ cid,
	show x,
	show y
    ]

plotGridDataToStrings :: PlotGridData -> [[String]]
plotGridDataToStrings = concatMap pointsData . M.toList
	    
type MesTypeCorrectionVals = Map MType Double

-- When subtracting background noise these are the minimal legal values.
minValMap :: MesTypeCorrectionVals
minValMap = M.fromList [("OD600",0.005), ("YFP", 20), ("MCHERRY",10),("CFP",10), ("GFP",10)]

constantBackgroundMap :: ExpData -> MesTypeCorrectionVals
constantBackgroundMap ed = M.fromList [(m, bg m) | m <- mes_types]
    where
	bg m = mean . map mVal . filterByType m $ bg_mes 
	mes_types = mesTypes bg_mes
	bg_mes = concat . filter (not . liveWell) . M.elems $ (ed ! mediaId)

subtractConstantBackground :: ExpData -> ExpData
subtractConstantBackground ed = M.map (M.map (map (\x -> x {mVal = corrected_mval x}))) ed
    where
	background_map = constantBackgroundMap ed
	corrected_mval m =
	    let mt = mType m in max (minValMap ! mt) (mVal m - (background_map ! mt))

mesTypes :: [Measurement] -> [String]
mesTypes = nub . map mType

expMesTypes :: ExpData -> [String]
expMesTypes = nub . map mType . concat . concatMap M.elems . M.elems

autoFluorescenceMap :: ExpData -> Maybe MesTypeCorrectionVals
autoFluorescenceMap ed = do
    af_colonies <- M.lookup wildTypeId ed
    let af_mes = M.elems af_colonies
    let mes_types = mesTypes . concat $ af_mes
    let af m = mean . map (expLevel' m) $ af_mes
    return . M.fromList $ [(m, af m) | m <- mes_types]

normalizePlate :: ExpData -> ExpData
normalizePlate ed
    | Nothing == M.lookup mediaId ed = M.map (M.map (map (\x -> if mType x == "OD600" then x {mVal = max (minValMap ! "OD600") (mVal x - stdMinOd)} else x))) $ ed
    | otherwise = subtractConstantBackground ed

changePlate :: Int -> Measurement -> Measurement
changePlate np ms = ms {mPlate = np}

createExpData :: [Measurement] -> ExpData
createExpData ms = fromList [ (label, m_for_label label) | label <- labels ms]
    where
	labels = nub . map mLabel
	colonies l = nub . map colonyId . filterBy mLabel l
	m_for_colony cid = filterBy colonyId cid ms
	m_for_label l = fromList [ (colony_id, m_for_colony colony_id) | colony_id <- colonies l ms ]

wellFromInts :: Int -> Int -> Well
wellFromInts r c = Well { wRow = ['a'..'h'] !! r, wColumn = c + 1 }

odLiveThreshold = 0.2
odThreshold = 0.005
stdMinOd = 0.04

verifySingleColony :: [Measurement] -> [Measurement]
verifySingleColony ms
    | null ms = error "empty list of measurements given"
    | 1 < (length . nub . map colonyId $ ms) = error $ "measurements for more than one colony given" ++ show ms
    | otherwise = ms

liveWell :: [Measurement] -> Bool -- returns whether measurements taken from a given well indicate that it grew.
liveWell ms = (last . valByTime "OD600" $ ms) > odLiveThreshold

mesByTime :: MType -> [Measurement] -> [Measurement]
mesByTime mt = sortBy (compare `on` mTime) . filterByType mt . verifySingleColony

valByTime :: MType -> [Measurement] -> [Double]
valByTime mt = map mVal . mesByTime mt

has :: (Eq a) => [a] -> a -> Bool
has = flip elem

timedMesData :: ExpData -> MType -> TimedPlotLinesData
timedMesData ed mt = M.map (M.map (map (\x -> (mVal x, mTime x)) . mesByTime mt)) (normalizePlate ed)

mesData :: ExpData -> MType -> PlotLinesData
mesData ed mt = M.map (M.map (map fst)) . timedMesData ed $ mt

mesToOd :: MType -> Maybe (Int,Int) -> [Measurement] -> [Double]
mesToOd mt m_range ms = map fst . timedMesToOd mt m_range $ ms

isLegal x = not $ isInfinite x || isNaN x

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
timedMesToOdData ed mt m_range = M.map (M.map (removeIllegalPoints . map (\(x,y) -> 
                    (logBase 10 x, y)) . timedMesToOd mt m_range)) nbg
    where
        nbg = removeDeadWells . normalizePlate $ ed
	
-- consider combining code with plotGridData (currently problem is requirement for specifying limits here)
mesToOdData :: ExpData -> MType -> Maybe (Int, Int) -> PlotLinesData
mesToOdData ed mt m_range = M.map (M.map (map fst)) . timedMesToOdData ed mt $ m_range
	
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
expLevels ed = M.map (M.map (\x -> [(m,if m == "OD600" then repeat . maxGrowth . od_mes $ x else mesToOd m Nothing x) | m <- mesTypes x])) ed
    where
        od_mes = sortBy (compare `on` snd) . map (\x -> (mVal x,toSeconds . mTime $ x)) . filter (\x -> mType x == "OD600")

subtractAutoFluorescence :: ExpData -> ExpLevelData -- need adjustment for df/dt and maximum value selection
subtractAutoFluorescence ed = M.map (M.map (map (\(mt,vals) -> (mt, map (corrected_el mt) vals)))) . expLevels $ ed
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
        grid_points = M.map (M.map (\x -> (exp_level xtype x,exp_level ytype $ x))) ned

growthGridData :: ExpData -> (String,String) -> AxesTrans -> PlotGridData
growthGridData ed (xtype,ytype) (fx,fy) = grid_points
    where
        data_sets = {-subtractAutoFluorescence . -}expLevels . removeDeadWells . normalizePlate $ ed
        plot_vals = M.map (M.map (map (\(mt,vals) -> (mt, calcexp . take 100 $ vals)))) data_sets
        grid_points = M.map (M.map (\x -> (fx $ (fromJust . lookup xtype $ x) + (fromJust . lookup ytype $ x),fromJust . lookup "OD600" $ x))) plot_vals
        calcexp = mean . take 3 . drop 2 . reverse . sort

intensityGridData' :: ExpData -> (String,String) -> AxesTrans -> PlotGridData
intensityGridData' ed (xtype,ytype) (fx,fy) = grid_points
    where
        data_sets = {-subtractAutoFluorescence . -}expLevels . removeDeadWells . normalizePlate $ ed
        plot_vals = M.map (M.map (map (\(mt,vals) -> (mt, calcexp vals)))) data_sets
        grid_points = M.map (M.map (\x -> (fx . fromJust . lookup xtype $ x,fy . fromJust . lookup ytype $ x))) plot_vals
        calcexp = mean . take 3 . drop 2 . reverse . sort

filterByWell :: Well -> [Measurement] -> [Measurement]
filterByWell = filterBy mWell

filterByType :: String -> [Measurement] -> [Measurement]
filterByType = filterBy mType

filterByWells :: [Well] -> [Measurement] -> [Measurement]
filterByWells ws = concat . zipWith filterByWell ws . repeat

--------------------------- Histograms
inRange :: (Num a, Ord a) => (a,a) -> a -> Bool
inRange (minV,maxV) v = v >= minV && v < maxV

-- counts value occurence of dots in bins_num evenly distributed in range.
histData :: (Double, Double) -> Int -> [Double] -> [(Double,Double)]
histData (min_val, max_val) bins_num dots = result
    where
	bins = map ((+) min_val . (*) bin_width . fromIntegral) [0..bins_num]
	bin_width = (max_val - min_val) / (fromIntegral bins_num)
	bin_count x = genericLength . filter (inRange (x,x+bin_width)) $ dots
	result = [ (x, bin_count x) | x <- bins]

-- Use the minimal and maximal values of the data for the range.
histDataDefRange :: Int -> [Double] -> [(Double,Double)]
histDataDefRange pn d = histData (minimum d, maximum d) pn d

-- Create a default Kernel Density histogram.
-- kdHist :: Int -> [Double] -> [(Double,Double)]
-- kdHist points_num dots = zip points_list values_list
--     where
-- 	(points,values) = KD.gaussianPDF points_num . DVU.fromList $ dots -- default KD
-- 	points_list = DVU.toList . KD.fromPoints $ points
-- 	values_list = DVU.toList values


----------- smoothing
windowSize = 5

type SmoothDataset = [Double] -> [Double]

changeVal :: Measurement -> Double -> Measurement
changeVal m x = m {mVal = x}

smoothMes :: SmoothDataset -> [Measurement] -> [Measurement]
smoothMes f ms = zipWith ($) (map changeVal ordered_data) . f . map mVal $ ordered_data
    where
	ordered_data = sortBy (compare `on` mTime) ms

applySmooth' :: SmoothDataset -> MType -> [Measurement] -> [Measurement]
applySmooth' f mt ms = not_affected ++ smoothed
    where
	(affected,not_affected) = partition ((==) mt . mType) ms
	smoothed = smoothMes f affected

applySmooth :: SmoothDataset -> MType -> ExpData -> ExpData
applySmooth f mt = M.map (M.map (applySmooth' f mt))

smoothAll :: SmoothDataset -> ExpData -> ExpData
smoothAll f ed = foldr (applySmooth f) ed ["OD600","MCHERRY","YFP","CFP"]

bFiltS :: SmoothDataset
bFiltS ds = mapMaybe (bFilt . take windowSize) . tails $ extended_dataset
    where
	extended_dataset = header ++ ds ++ trailer
	header = replicate buf_size . head $ ds
	trailer = replicate buf_size . last $ ds
	buf_size = windowSize `div` 2

bFilt :: [Double] -> Maybe Double
bFilt xs
    | length xs < windowSize = Nothing
    | null relevant_data = val xs
    | otherwise = val relevant_data
	where
	    mx = maximum xs
	    mn = minimum xs
	    val = Just . mean 
	    relevant_data = filter (\x -> x < mx && x > mn) $ xs

wellStr :: Well -> String
wellStr w = concat ["(", [wRow w],",",show . wColumn $ w,")"]
