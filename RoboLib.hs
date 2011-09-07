module RoboLib (
    Well (..),
    ColonyId (..),
    wellFromInts,
    Measurement (..),
    plotIntensityGrid,
    plotMesToOD,
    plotMesData,
    plotData,
    plotGrid,
    mesData,
    mesDataTime,
    mesToOdData,
    intensityGridData,
    plotGridDataToStrings,
    plotLinesDataToStrings,
    PlotLinesData,
    PlotGridData,
    loadExpData,
    ExpData,
    createExpData,
    ExpId,
    MType,
    expLevel,
    noTrans,
    expMesTypes,
    smoothAll,
    bFiltS,
    has,
    Label,
    wellStr,
    )
where
import Graphics.Gnuplot.Simple
import Text.ParserCombinators.Parsec
import Data.CSV
import Data.Either (rights)
import Data.Function (on)
import Data.List
import Data.Map ((!), Map(..))
import Data.Maybe
import Statistics.Sample (mean, stdDev)
import Data.DateTime (fromSeconds, toSqlString, DateTime, toSeconds)
import qualified Data.Vector.Unboxed as DVU
import qualified Statistics.KernelDensity as KD
import qualified Char as C
import qualified Data.Map as M
import qualified Data.Vector as V

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

type ExpData = M.Map Label (M.Map ColonyId [Measurement]) -- experiment data is mapped like this.

type ExpLevelData = M.Map Label (M.Map ColonyId [(MType,[Double])]) -- expression levels for each colony and measurement type.

type PlotLinesData = M.Map Label (M.Map ColonyId [Double]) -- for each label - a list of colonies, for each colony - a line.
type PlotGridData = M.Map Label (M.Map ColonyId (Double,Double)) -- for each label - a list of colonies, for each colony - a line.

-- utils for outputting plot data to files
linesData :: (String,M.Map ColonyId [Double]) -> [[String]]
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

pointsData :: (String,M.Map ColonyId (Double,Double)) -> [[String]]
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
minValMap = M.fromList [("OD600",0), ("YFP", 1), ("MCHERRY",1),("CFP",1)]

constantBackgroundMap :: ExpData -> MesTypeCorrectionVals
constantBackgroundMap ed = M.fromList [(m, bg m) | m <- mes_types]
    where
	bg m = meanL . map mVal . filterByType m $ bg_mes 
	mes_types = mesTypes bg_mes
	bg_mes = concat . M.elems $ (ed ! mediaId)

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

meanL :: [Double] -> Double
meanL = mean . V.fromList

autoFluorescenceMap :: ExpData -> Maybe MesTypeCorrectionVals
autoFluorescenceMap ed = do
    af_colonies <- M.lookup wildTypeId ed
    let af_mes = M.elems af_colonies
    let mes_types = mesTypes . concat $ af_mes
    let af m = meanL . map (expLevel m) $ af_mes
    return . M.fromList $ [(m, af m) | m <- mes_types]

normalizePlate :: ExpData -> ExpData
normalizePlate ed
    | Nothing == M.lookup mediaId ed = ed
    | otherwise = subtractConstantBackground ed

changePlate :: Int -> Measurement -> Measurement
changePlate np ms = ms {mPlate = np}

wellFromInts :: Int -> Int -> Well
wellFromInts r c = Well { wRow = ['a'..'h'] !! r, wColumn = c + 1 }

maxMes = 70000

readExpLine :: [String] -> Measurement
readExpLine m = Measurement { 
		    mExpDesc = m !! 0,
		    mPlate = read $ m !! 1,
		    mType = m !! 2,
		    mTime = fromSeconds . read $ m !! 3,
		    mWell = (wellFromInts `on` read) (m !! 4) (m !! 5),
		    mLabel = m !! 6,
		    mVal = if m !! 7 == "nan" then maxMes else read (m !! 7)
		}

-- assume CSV format of each row is:
-- expId, plate, measurement_type, timestamp, column, row, label, value
loadExpData :: FilePath -> IO ExpData
loadExpData filename = do
    file_data' <- parseFromFile csvFile filename
    let file_data = head . rights . return $ file_data'
    let ms = map readExpLine $ file_data
    return . createExpData $ ms

createExpData :: [Measurement] -> ExpData
createExpData ms = M.fromList [ (label, m_for_label label) | label <- labels ms]
    where
	labels = nub . map mLabel
	colonies l = nub . map colonyId . filterBy mLabel l
	m_for_colony cid = filterBy colonyId cid ms
	m_for_label l = M.fromList [ (colony_id, m_for_colony colony_id) | colony_id <- colonies l ms ]

odLiveThreshold = 0.2
odThreshold = 0.06
odMaxT = 0.15

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

makePlotData :: (String, [[Double]]) -> Int -> [(PlotStyle, [Double])]
makePlotData (desc, vals) c = [ (defaultStyle {lineSpec = CustomStyle [LineTitle desc, LineType c]},x) | x <- vals ]

makePlotGridData :: (String,[(Double,Double)]) -> Int -> (PlotStyle, [(Double,Double)])
makePlotGridData (label,points) c =
    (defaultStyle {plotType = Points, lineSpec = CustomStyle [LineTitle label, PointType c, PointSize 2]},points)

has :: (Eq a) => [a] -> a -> Bool
has = flip elem

plotData :: String -> PlotLinesData -> Maybe FilePath -> IO ()
plotData title pld m_fn = do
    let by_label = [ (label, M.elems x) | (label,x) <- M.toList pld ]
    let plot_data = concat . zipWith makePlotData by_label $ [1..]
    let fileoptions = fromMaybe [] . fmap fileOpts $ m_fn
    plotListsStyle ([Title title] ++ fileoptions) plot_data

mesDataTime :: ExpData -> MType -> M.Map Label (M.Map ColonyId [(Double,DateTime)])
mesDataTime ed mt = M.map (M.map (map (\x -> (mVal x, mTime x)) . mesByTime mt)) ed

mesData :: ExpData -> MType -> PlotLinesData
mesData ed mt = M.map (M.map (valByTime mt)) ed

plotMesData :: ExpData -> MType -> Maybe FilePath -> IO()
plotMesData ed  mt m_fn = plotData mt (mesData ed mt) m_fn

mesToOd :: MType -> Maybe (Int,Int) -> [Measurement] -> [Double]
mesToOd mt Nothing ms = mesToOd mt (Just . mesLimits mt $ ms) $ ms 
mesToOd mt (Just (low,high)) ms = m_to_od_vals
    where
	vals t = take (high - low) . drop low . valByTime t
	diffs x = zipWith (-) (tail x) x
	m_to_od_vals = zipWith (/) (diffs . vals mt $ ms) . vals "OD600" $ ms

removeDeadWells :: ExpData -> ExpData
removeDeadWells = M.filter (not . M.null) . M.map (M.filter liveWell)

-- consider combining code with plotGridData (currently problem is requirement for specifying limits here)
mesToOdData :: ExpData -> MType -> Maybe (Int, Int) -> PlotLinesData
mesToOdData ed mt m_range = M.map (M.map (map (logBase 10) . mesToOd mt m_range)) nbg
    where
	nbg = removeDeadWells . normalizePlate $ ed
	
plotMesToOD :: ExpData -> MType -> Maybe (Int, Int) -> Maybe FilePath -> IO()
plotMesToOD ed mt m_range m_fn = plotData (mt ++ " to OD") (mesToOdData ed mt m_range) m_fn

minIdx :: (Ord a) => [a] -> Int
minIdx xs = fromJust . findIndex ((==) . minimum $ xs) $ xs

odLimits :: [Double] -> (Int,Int) -- assumes time-wise sorted list of OD measurements.
odLimits ods = (low,high)
    where
	rods = reverse ods
	low = length ods - (fromMaybe (minIdx rods) . findIndex (< odThreshold) $ rods)
	high = length ods -- (fromJust . findIndex (< upper_limit ods) $ rods)
	upper_limit x = (minimum x + maximum x) / 2

mesLimits :: MType -> [Measurement] -> (Int,Int)
mesLimits mt ms = (low,high)
    where
	(low,high_od) = odLimits . valByTime "OD600" $ ms
	high = min high_od . fromMaybe (high_od) . findIndex (>= maxMes) . valByTime mt $ ms

expLevel :: MType -> [Measurement] -> Double
expLevel mt = meanL . mesToOd mt Nothing

expLevels :: ExpData -> ExpLevelData
expLevels ed = M.map (M.map (\x -> [(m,mesToOd m Nothing x) | m <- mesTypes x])) ed

subtractAutoFluorescence :: ExpData -> ExpLevelData -- need adjustment for df/dt and maximum value selection
subtractAutoFluorescence ed = M.map (M.map (map (\(mt,vals) -> (mt, map (corrected_el mt) vals)))) . expLevels $ ed
    where
	corrected_el mt x = fromMaybe x $ do
	    afm <- autoFluorescenceMap ed
	    af <- M.lookup mt afm
	    return $ max (minValMap ! mt) (x - af)

fileOpts :: String -> [Attribute]
fileOpts fn = [ Custom "terminal" ["png", "size 1000,1000"], Custom "output" ["\"" ++ fn ++ "\""]]

type AxesTrans = ((Double -> Double),(Double -> Double))

noTrans = (id,id)

intensityGridData :: ExpData -> (String,String) -> AxesTrans -> PlotGridData
intensityGridData ed (xtype,ytype) (fx,fy) = grid_points
    where
	data_sets = {-subtractAutoFluorescence . -}expLevels . removeDeadWells . normalizePlate $ ed
	plot_vals = M.map (M.map (map (\(mt,vals) -> (mt, calcexp vals)))) data_sets
	grid_points = M.map (M.map (\x -> (fx . fromJust . lookup xtype $ x,fy . fromJust . lookup ytype $ x))) plot_vals
	calcexp = maximum -- . map (meanL . take 3) . tails -- this is the strategy for calculating the expression level from its distinct values. it takes the maximum of the averages over windows of size 3.

plotGrid :: String -> PlotGridData -> (String,String) -> Maybe FilePath -> IO ()
plotGrid title pgd (xtype,ytype) m_fn = plotPathsStyle plot_attrs plot_lines
    where
	file_options = fromMaybe [] . fmap fileOpts $ m_fn
	plot_attrs = [XLabel xtype, YLabel ytype, XRange (0,7), YRange (0,7)] ++ file_options
	labeled_grid_points = [ (label,M.elems pm) | (label,pm) <- M.toList pgd ]
	plot_lines = zipWith makePlotGridData labeled_grid_points [1..]

plotIntensityGrid :: ExpData -> (String, String) -> AxesTrans -> Maybe FilePath -> IO ()
plotIntensityGrid ed axes fs m_fn = plotGrid "Log scale grid plot" pgd axes m_fn
    where
	pgd = intensityGridData ed axes fs

filterBy :: (Eq b) => (a -> b) -> b -> [a] -> [a]
filterBy f v = filter ((==) v . f)

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
kdHist :: Int -> [Double] -> [(Double,Double)]
kdHist points_num dots = zip points_list values_list
    where
	(points,values) = KD.gaussianPDF points_num . DVU.fromList $ dots -- default KD
	points_list = DVU.toList . KD.fromPoints $ points
	values_list = DVU.toList values


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
	    val = Just . meanL 
	    relevant_data = filter (\x -> x < mx && x > mn) $ xs

wellStr :: Well -> String
wellStr w = concat ["(", [wRow w],",",show . wColumn $ w,")"]

