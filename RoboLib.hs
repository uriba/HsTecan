module RoboLib (
    subMinVal,
    Well (..),
    Measurement (..),
    loadPlateExpData,
    histData,
    histDataDefRange,
    kdHist,
    groupByColony,
    plotMesData,
    plotMesDataByGroup,
    wellColumn,
    control,
    )
where
import Graphics.Gnuplot.Simple
import Text.ParserCombinators.Parsec
import Data.CSV
import Data.Either (rights)
import Data.Function (on)
import Data.List
import Data.Map ((!))
import Data.Maybe
import Statistics.Sample (mean, stdDev)
import Data.DateTime (fromSeconds, toSqlString, DateTime, toSeconds)
import qualified Data.Vector.Unboxed as DVU
import qualified Statistics.KernelDensity as KD
import qualified Char as C
import qualified Data.Map as M
import qualified Data.Vector as V
import System (getArgs)

-- reduce minimal value from entire matrix assuming it is inherent background (mostly usable for OD).
subMinVal :: [[Double]] -> [[Double]]
subMinVal x = map (map (-min_val +)) x
    where
	min_val = minimum . map minimum $ x

-- Some types to make life easier...
type PlateId = String
data Well = Well { wRow :: Char , wColumn :: Int } deriving (Eq, Show, Ord)
data ColonyId = ColonyId { cPlate :: PlateId, cWell :: Well } deriving (Eq, Show, Ord)

data Measurement = Measurement { mColonyId :: ColonyId, mTime :: DateTime , mType :: String, mDesc :: String, mVal :: Double } deriving (Eq, Show)

data WellType = JustMedia | NoFluorescence | Specimen { sID :: String }
type PlateDescription = M.Map WellType [Well]
-- how to do the control? first normalize all od's by JustMedia.
-- Then consider normalizing all flourescence by Just media as well?
-- Then calculate auto fluorescence of NoFluorescence wells (calculate expression level by deviding reads in od)
-- subtract auto-fluorescence from all expression level measurements (meaning first calculate fl/od and then subtract auto-fl val).

changePlate :: String -> Measurement -> Measurement
changePlate np ms = ms {mColonyId = ((mColonyId ms) {cPlate = np})}

mWell :: Measurement -> Well
mWell = cWell . mColonyId

mPlate :: Measurement -> String
mPlate = show . cPlate . mColonyId

wellFromStr :: String -> Well
wellFromStr s = Well { wRow = ['a'..'h'] !! fst well_ind , wColumn = snd well_ind + 1 }
    where well_ind = read s :: (Int, Int)

readExpLine :: [String] -> Measurement
readExpLine m = Measurement { 
		    mColonyId = ColonyId { cPlate = m !! 0, cWell = wellFromStr (m !! 3) },
		    mTime = fromSeconds . read $ m !! 2,
		    mType = m !! 1,
		    mDesc = "",
		    mVal = if m !! 4 == "nan" then 70000 else read (m !! 4)
		}

-- assume CSV format is each row is:
-- plate_num,measurement_type,timestamp, (column,row), value
loadPlateExpData :: FilePath -> IO [Measurement]
loadPlateExpData filename = do
    file_data' <- parseFromFile csvFile filename
    let file_data = head . rights . return $ file_data'
    return . map readExpLine . tail $ file_data -- first entry is the column description and cannot be parsed

wellColumn :: Int -> [Well]
wellColumn = zipWith Well ['a'..'h'] . repeat

allWells96 = concatMap wellColumn [1..12]

odLiveThreshold = 0.2
odThreshold = 0.08
odBackground = 0.04

liveWell :: [Measurement] -> Bool -- returns whether measurements taken from a given well indicate that it grew.
liveWell ms
    | 1 < (length . nub . map mWell $ ms) = error $ "measurments for more than one well given" ++ show ms
    | null ms = False
    | otherwise = last sorted_vals > odLiveThreshold
	where
	    sorted_vals =  map mVal . sortBy (compare `on` mTime) . filterByType "OD600" $ ms

liveWells :: [Measurement] -> [Well]
liveWells ms = [ x | x <- allWells96, liveWell . filterByWell x $ ms] 

findRelevantODTimes :: [Measurement] -> (DateTime, DateTime)
findRelevantODTimes ms = (min_time, max_time)
    where
	min_time = mTime . head . dropWhile (\x -> mVal x >= lower_threshold) $ reversed_sorted_m
	max_time = mTime . last . takeWhile (\x -> mVal x >= upper_threshold) $ reversed_sorted_m
	reversed_sorted_m = reverse . sortBy (compare `on` mTime) $ ms
	vals = map mVal ms
	min_val = minimum vals
	max_val = maximum vals
	lower_threshold = min_val + ((max_val - min_val)/3)
	upper_threshold = min_val + ((max_val - min_val)/2)

filterRelevantMesByOptOD :: [Measurement] -> [Measurement] -- assumes measurements are for the same plate/Well
filterRelevantMesByOptOD ms = filter (\x -> inRange times . toSeconds . mTime $ x) ms
    where
	times = (toSeconds min_time, toSeconds max_time)
	(min_time, max_time) = findRelevantODTimes . filterByType "OD600" $ ms

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

groupByColony :: [Measurement] -> [[Measurement]]
groupByColony = groupBy ((==) `on` mColonyId) . sortBy (compare `on` mColonyId)

sortMesByTime :: String -> [Measurement] -> [Measurement]
sortMesByTime m_type = sortBy (compare `on` mTime) . filterByType m_type

timeConsecMesByPlateWell :: String -> [Measurement] -> [[Double]]
timeConsecMesByPlateWell m_type  ms = map (map mVal . sortMesByTime m_type) . groupByColony $ ms

plotMesData :: [Measurement] -> String -> IO ()
plotMesData ms m_type = do
    let m_vals = timeConsecMesByPlateWell m_type ms
    plotLists [Title ("plotting:" ++ m_type)] m_vals

containsElem :: (Eq a) => [a] -> a -> Bool
containsElem = flip elem

makePlotData :: (String, [[Double]]) -> Int -> [(PlotStyle, [Double])]
makePlotData (desc, vals) c = [ (defaultStyle {lineSpec = CustomStyle [LineTitle desc, LineType c]},x) | x <- vals ]

plotMesDataByGroup :: [Measurement] -> [(String,[Well])] -> String -> Maybe FilePath -> IO()
plotMesDataByGroup ms groups m_type mfn = do
    let by_desc = [ (desc, timeConsecMesByPlateWell m_type . filter ((wells `containsElem`) . mWell) $ ms) | (desc,wells) <- groups ]
    let plot_data = concat . zipWith makePlotData by_desc $ [1..]
    let fileoptions = if isJust mfn
			then [ Custom "terminal" ["svg"], Custom "output" ["\"" ++ fromJust mfn ++ "\""]]
			else []
    plotListsStyle ([Title ("plotting:" ++ m_type)] ++ fileoptions) plot_data

plotMesDataFuncByGroup :: [Measurement] -> [(String,[Well])] -> String -> (Double -> Double) -> IO()
plotMesDataFuncByGroup ms groups m_type f = do
    let by_desc = [ (desc, map (map f) . timeConsecMesByPlateWell m_type . filter ((wells `containsElem`) . mWell) $ ms) | (desc,wells) <- groups ]
    let plot_data = concat . zipWith makePlotData by_desc $ [1..]
    plotListsStyle [Title ("plotting:" ++ m_type)] plot_data


makePlotODData ::  (Int, Int) -> (String, [[Double]]) -> (String, [[Double]]) -> Int -> [(PlotStyle, [Double])]
makePlotODData (low, len) (desc1, od_vals) (desc2, vals) c
    | desc1 == desc2 = [ (defaultStyle {lineSpec = CustomStyle [LineTitle desc1, LineType c]},x) | x <- plot_vals ]
    | otherwise = error $ "differing descriptions:" ++ desc1 ++ " and " ++ desc2
    where
	plot_vals  = map ( take len . drop low ) $ map (map (logBase 10)) $ zipWith (zipWith (/)) vals (subMinVal od_vals)

plotMesToODByGroup :: [Measurement] -> [(String,[Well])] -> String -> (Int, Int) -> Maybe FilePath -> IO()
plotMesToODByGroup ms groups m_type (low, len) mfn = do
    let by_desc = [ (desc, timeConsecMesByPlateWell m_type . filter ((wells `containsElem`) . mWell) $ ms) | (desc,wells) <- groups ]
    let od_by_desc = [ (desc, timeConsecMesByPlateWell "OD600" . filter ((wells `containsElem`) . mWell) $ ms) | (desc,wells) <- groups ]
    let plot_data = concat . zipWith3 (makePlotODData (low, len)) od_by_desc by_desc $ [1..]
    let fileoptions = if isJust mfn
			then [ Custom "terminal" ["svg"], Custom "output" ["\"" ++ fromJust mfn ++ "\""]]
			else []
    plotListsStyle ([Title ("plotting:" ++ m_type ++ " to OD")] ++ fileoptions) plot_data

mesOfWell :: Well -> [Measurement] -> [Measurement]
mesOfWell w ms = filterByWell w ms

minIdx :: (Ord a) => [a] -> Int
minIdx xs = fromJust . findIndex ((==) . minimum $ xs) $ xs

odLimits :: [Double] -> (Int,Int) -- assumes list of OD measurements sorted time-wise.
odLimits ms = (low,high)
    where
	rms = reverse ms
	low = length ms - (fromMaybe (minIdx rms) . findIndex (< odThreshold) $ rms)
	high = length ms - (fromJust . findIndex (< upper_limit ms) . reverse $ ms)
	upper_limit x = (minimum x + maximum x / 2)

maxMes = 70000

expLevel :: Well -> [Measurement] -> String -> Double
expLevel w ms mt = mean . V.fromList $ exp_levels
    where
	wms = filterByWell w ms
	od_mes = filterByType "OD600" wms
	t_mes =  filterByType mt wms
	vals = map mVal . sortBy (compare `on` mTime)
	ods = vals od_mes
	(low,high_od) = odLimits ods
	high = min high_od . fromMaybe (length t_mes) . findIndex (>= maxMes) . vals $ t_mes
	norm_ods = map (\x -> x - odBackground) ods
	rel_mes = take (high - low) . drop low
	exp_levels = map (logBase 10) . zipWith (/) (rel_mes . vals $ t_mes) $ (rel_mes norm_ods)

expLevelPerPlate :: [Measurement] -> [String] -> [Well] -> [(Well,[(String,Double)])]
expLevelPerPlate ms mt wells = [ (w,zip mt . map (expLevel w ms) $ mt) | w <- wells ]

makePlotGridData :: [(Double,Double)] -> String -> Int -> (PlotStyle, [(Double,Double)])
makePlotGridData data_set plate c =
    (defaultStyle {plotType = Points, lineSpec = CustomStyle [LineTitle plate, PointType c, PointSize 2]},data_set)

intensityGridPoints :: (String, String) -> [Measurement] -> [(Double,Double)]
intensityGridPoints (xlabel, ylabel) ms = [ (snd . head . snd $ x, snd . last . snd $ x) | x <- exp_levels]
    where
	exp_levels = expLevelPerPlate ms [xlabel,ylabel] (liveWells ms)

plotIntensityGrid :: [Measurement] -> (String, String) -> Maybe FilePath -> IO ()
plotIntensityGrid ms (xtype,ytype) mfn = plotPathsStyle plot_attrs plot_lines
    where
	data_sets = map (intensityGridPoints (xtype,ytype)) $ by_plate
	by_plate = groupBy ((==) `on` mPlate) ms -- . sortBy (compare `on` mPlate) $ ms
	plates = map (mPlate . head) by_plate
	plot_lines = zipWith3 makePlotGridData data_sets plates [1..]
	plot_attrs = [XLabel xtype, YLabel ytype, XRange (0,7), YRange (0,7)] ++ file_options
	file_options = if isJust mfn
			then [ Custom "terminal" ["svg", "size 1000,1000"], Custom "output" ["\"" ++ fromJust mfn ++ "\""]]
			else []
minMesVal = 1 -- this is the minimal legal value of a measurement. this needs further inspection.

filterBy :: (Eq b) => (a -> b) -> b -> [a] -> [a]
filterBy f v = filter ((==) v . f)

filterByWell = filterBy mWell
filterByType = filterBy mType

avgVal :: String -> [Well] -> [Measurement] -> Double
avgVal m_type cWells mes = mean . V.fromList $ cmes
    where
	cmes = map mVal . by_mtype . by_wells cWells $ mes
	by_wells ws = filter (\x -> mWell x `elem` ws)
	by_mtype = filterByType m_type

normalizeReads :: String -> [Well] -> Double -> [Measurement] -> [Measurement]
normalizeReads m_type cWells min_val mes = [ x {mVal = new_val x} | x <- mes] 
    where
	new_val x = if mType x == m_type then corrected_val x else mVal x
	corrected_val x = max min_val (mVal x - avg_val)
	avg_val = avgVal m_type cWells mes

plotIntensityGridByGroup :: [Measurement] -> [(String,[Well])] -> (String,String) -> Maybe FilePath -> IO ()
plotIntensityGridByGroup ms gr vals fp
    | (length . nub . map mPlate $ ms) > 1 = error "this function works only on single plate measurements"
    | otherwise = plotIntensityGrid (concat . zipWith renameByWell gr . repeat $ ms) vals fp
	where
	    renameByWell (name,wells) ms = map (changePlate name) . filter (\x -> mWell x `elem` wells) $ ms

control :: Int -> [(String,[Well])]
control i = [ (name i ++ [x], zipWith Well (repeat x) . cont_ind $ i) | x <- ['a'..'h']]
    where
	name i = ["MCherry-","YFP-","CFP-"] !! ((i - 1))
	cont_ind i = zipWith (+) [1,2,3,4] . repeat $ (i - 1) * 4 

control2 :: [(String,[Well])]
control2 = sort $ ("YFP-a", zipWith Well (repeat 'a') [1..6]) : 
	    [ ("YFP-" ++ [C.chr $ 1 + C.ord x], zipWith Well (repeat x) [1..6]) | x <- ['b'..'e']] ++
	    [ ("YFP-b", zipWith Well (repeat 'f') [1..6]) ]

bestOfBest2 :: [(String,[Well])]
bestOfBest2 = [
		("mA",[Well 'a' 11, Well 'a' 12]),
		("mB",[Well 'b' 11, Well 'b' 12]),
		("mC",[Well 'c' 11, Well 'c' 12]),
		("mD",[Well 'd' 11, Well 'd' 12]),
		("yA",[Well 'e' 12]),
		("yB",[Well 'f' 12]),
		("yC",[Well 'g' 12]),
		("yD",[Well 'h' 12]),
		("anchor-D4(A?)",[Well 'e' 11]),
		("anchor-G1(AD)",[Well 'f' 11]),
		("anchor-A11(??)",[Well 'g' 11]),
		("anchor-G11(CA)",[Well 'h' 11]),
		("Low - stop codon (C7,C8,E7,H11)",[Well 'f' 1,Well 'h' 2, Well 'e' 4, Well 'a' 4]),
		("AD", [Well 'a' 1, Well 'a' 3, Well 'b' 5]),
		("CA", [Well 'd' 3, Well 'b' 2]),
		("DA", [Well 'g' 5, Well 'd' 1]),
		("DC", [Well 'f' 4, Well 'c' 6, Well 'd' 4]),
		("DD", [Well 'f' 5, Well 'g' 3]),
		("CD", [Well 'h' 4, Well 'b' 3, Well 'b' 4, Well 'd' 5,Well 'e' 2]),
		("DDframeshift", [Well 'h' 1]),
		("CstopC", [Well 'e' 1]),
		("CFP-A", [Well 'a' 10]),
		("Inert", [Well 'e' 6, Well 'f' 6, Well 'g' 6]),
		("New Colonies", concatMap (zipWith Well ['a'..'h'] . repeat) [7..10] \\ [Well 'a' 10]),
		("Unmapped", [Well 'a' 2,Well 'a' 5,Well 'a' 6,Well 'b' 1,Well 'b' 6,Well 'c' 1,Well 'c' 2,Well 'c' 3,Well 'c' 4,Well 'c' 5,Well 'd' 2,Well 'd' 6,Well 'e' 3,Well 'e' 5,Well 'f' 2,Well 'f' 3,Well 'g' 1,Well 'g' 2,Well 'g' 4,Well 'h' 3,Well 'h' 5,Well 'h' 6])]

bObSimple :: [(String,[Well])]
bObSimple = [
		("mA-D",[Well 'a' 11, Well 'a' 12,Well 'b' 11, Well 'b' 12, Well 'c' 11, Well 'c' 12,Well 'd' 11, Well 'd' 12]),
		("yA-D",[Well 'e' 12,Well 'f' 12,Well 'g' 12,Well 'h' 12]),
		("Stop codon",[Well 'f' 1,Well 'h' 2, Well 'e' 4, Well 'a' 4]),
		("AD", [Well 'a' 1, Well 'a' 3, Well 'b' 5, Well 'f' 11]),
		("CA", [Well 'd' 3, Well 'b' 2, Well 'h' 11]),
		("DA", [Well 'g' 5, Well 'd' 1]),
		("DC", [Well 'f' 4, Well 'c' 6, Well 'd' 4]),
		("DD", [Well 'f' 5, Well 'g' 3, Well 'h' 1]),
		("CD", [Well 'h' 4, Well 'b' 3, Well 'b' 4, Well 'd' 5,Well 'e' 2]),
		("CC", [Well 'e' 1]),
		("Inert", [Well 'e' 6, Well 'f' 6, Well 'g' 6, Well 'a' 10]),
		("Unmapped", [Well 'a' 2,Well 'a' 5,Well 'a' 6,Well 'b' 1,Well 'b' 6,Well 'c' 1,Well 'c' 2,Well 'c' 3,Well 'c' 4,Well 'c' 5,Well 'd' 2,Well 'd' 6,Well 'e' 3,Well 'e' 5,Well 'f' 2,Well 'f' 3,Well 'g' 1,Well 'g' 2,Well 'g' 4,Well 'h' 3,Well 'h' 5,Well 'h' 6,Well 'e' 11, Well 'g' 11] ++ (concatMap (zipWith Well ['a'..'h'] . repeat) [7..10] \\ [Well 'a' 10]))]

pairsAC206 :: [(String,[Well])]
pairsAC206 = [
		("AC",zipWith Well ['a'..'h'] (repeat 1)),
		("BC",zipWith Well ['a'..'h'] (repeat 2)),
		("CC",zipWith Well ['a'..'h'] (repeat 3)),
		("DC",zipWith Well ['a'..'h'] (repeat 4)),
		("EC",zipWith Well ['a'..'h'] (repeat 5)),
		("AC",zipWith Well ['a'..'h'] (repeat 6)),
		("YA", zipWith Well ['a','b'] (repeat 7)),
		("YB", zipWith Well ['c','d'] (repeat 7)),
		("YC", zipWith Well ['e','f'] (repeat 7)),
		("YD", zipWith Well ['g','h'] (repeat 7)),
		("YE", zipWith Well ['a','b'] (repeat 8)),
		("YZ", zipWith Well ['c','d'] (repeat 8)),
		("mA", zipWith Well ['e','f'] (repeat 8)),
		("mB", zipWith Well ['g','h'] (repeat 8)),
		("mC", zipWith Well ['a','b'] (repeat 9)),
		("mD", zipWith Well ['c','d'] (repeat 9)),
		("mE", zipWith Well ['e','f'] (repeat 9)),
		("mZ", zipWith Well ['g','h'] (repeat 9)),
		("inert", zipWith Well ['a','b'] (repeat 10)),
		("media", zipWith Well ['c','d'] (repeat 10)),
		("empty", zipWith Well ['e','f'] (repeat 10)),
		("mE",zipWith Well ['a'..'h'] (repeat 11))
	    ]
		
--plotMesOptODKDHist :: [Measurement] -> String -> IO ()
--plotMesOptODKDHist ms m_type = do
{-
plotMesOptODHist :: [Measurement] -> String -> IO ()
plotMesOptODHist ms m_type = do
    let mes_at_opt_od = map pickMesByOptOD . groupByColony $ ms
    let od_data = getMesVals mes_at_opt_od "OD600"
    let mt_data = getMesVals mes_at_opt_od m_type
    let dots = map (logBase 10) . zipWith (/) mt_data $ od_data
    let points_num = 30
    let hist_data = histDataDefRange points_num dots
    plotPathStyle [Title (m_type ++ "/od,  log (base 10)," ++ show points_num ++ " steps histogram")]
	(defaultStyle { plotType = Boxes}) hist_data

-}
{- probably not relevant anymore
control :: Int -> [(String,[Well])]
control i 
    | i == 2 = [ (name i ++ [x], zipWith Well (repeat x) . cont_ind $ i) | x <- ['a'..'c']]
	    ++ [ (name i ++ [x], zipWith Well (repeat x) . cont_ind $ i) | x <- ['e'..'g']]
	    ++ [ (name i ++ "d", zipWith Well ['d','g'] [4,4])]
    | otherwise = [ (name i ++ [x], zipWith Well (repeat x) . cont_ind $ i) | x <- ['a'..'h']]
    where
	name i = ["MCherry-","YFP-","CFP-"] !! ((i - 1) `mod` 3)
	cont_ind i = zipWith (+) [1,2] . repeat $ (i - 1) * 2 
-}
--plotRBGData :: [Double] -> [Int] -> String -> [(String,[[Double]])] -> IO()
--plotRBGData ods' indices rgb_type full_rgb_data = do
    --let drop_index = 54
    --let ods = take drop_index ods' ++ drop (drop_index + 1) ods'
    --let good_od_indices = findIndices (> 0.1) ods
    --putStrLn $ "taking " ++ (show . length $ good_od_indices) ++ " indices"
    --let good_ods = map (\x -> ods !! x) good_od_indices
    --let rgb_data' = subMinVal . fromJust . lookup rgb_type $ full_rgb_data
    --let rgb_data = take drop_index rgb_data' ++ drop (drop_index + 1) rgb_data'
    --plotLists [Title ("normalized time measurements of:" ++ rgb_type) ] rgb_data
    --let points_num = 30
    --let relevant_rgb_data = zipWith (!!) rgb_data indices
    --let good_relevant_rgb_data = map (\x -> relevant_rgb_data !! x) good_od_indices
    --let dots = map (logBase 10) . zipWith (/) relevant_rgb_data $ ods
    --let control_od = last ods
    --putStrLn $ "Control OD is:" ++ show control_od
--    let control_rgb = last relevant_rgb_data
--    let control_dot = logBase 10 (control_rgb / control_od)
--    let hist_data = histData points_num dots
--    let control_data = [fst . head $ hist_data, fst . last $ hist_data] ++ (take 5 . repeat $ control_dot)
--    let control_hist = histData points_num control_data 
--    plotPathsStyle [Title (rgb_type ++ "/od,  log (base 10)," ++ show points_num ++ " steps histogram")]
--	[(defaultStyle { plotType = Boxes}, hist_data), (defaultStyle {plotType = Boxes}, control_hist) ]
--
--main = do
    --args <- getArgs
    --let plate_num = head args
    --let file_name = "/home/uri/work/UriNivExp/output-" ++ plate_num
    --ex_data <- parseOutputFile file_name 
    --let od_data = subMinVal . fromJust . lookup "OD600" $ ex_data
    --let relevant_indices = map findRelevantOdIndex od_data
    --let relevant_ods = zipWith (!!) od_data relevant_indices
    --plotLists [] od_data
    --plotRBGData relevant_ods relevant_indices "YFP" ex_data
    --plotRBGData relevant_ods relevant_indices "MCHERRY" ex_data
    --plotRBGData relevant_ods relevant_indices "CFP" ex_data

-- get plate number.
-- plot od graphs for reference.
-- plot three KD graphs, corresponding to RGB reads.
--processCsvVals :: String -> [[String]] -> [[Double]]
--processCsvVals mes_type file_data = result
    --where
	--relevant_entries = filter (\x -> (x !! 1) == mes_type) file_data
	--plate_reads pn e = map (read . last) . filter (\x -> (pn `div` 12, pn `mod` 12) == read (x !! 3)) $ e
	--result = zipWith plate_reads [0..95] . repeat $ relevant_entries
	
-- generates (x,y) value graph using KernelDensity from the list of samples given.
--kdData :: Int -> [Double] -> [(Double,Double)]
--kdData points_num dots = zip points_list values_list
    --where
	--points = KD.choosePoints points_num 0.07 . DVU.fromList $ dots
	--values = KD.estimatePDF KD.gaussianKernel 0.07 (DVU.fromList dots) points
	---- (points,values) = KD.simplePDF KD.gaussianBW KD.gaussianKernel 3 points_num . DVU.fromList $ dots
	--points_list = DVU.toList . KD.fromPoints $ points
	--values_list = DVU.toList values
-- designate control wells and plot them on the side?
-- filter out wells with irrelevant od data.
-- plot by list of wells
-- process 'nan' values (find out what they mean from xml)
-- include functions for plotting of RGB/OD ratio over time.
-- include functions to manipulate measurements? (subtracting minimum, using control etc.)
-- include functions to easily plot by well, or group of wells for control.
