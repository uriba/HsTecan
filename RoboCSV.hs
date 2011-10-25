module RoboCSV (
    loadExpData,
    linesDataToCSV,
    gridDataToCSV,
)
where
import Text.ParserCombinators.Parsec
import Data.CSV
import Data.Either (rights)
import RoboLib (Measurement(..), ExpData(..), ColonyId, colonyId, wellFromInts, createExpData, maxMes, PlotGridData, PlotLinesData)
import Data.DateTime (fromSeconds, toSqlString)
import Data.List (nub)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as M
import Biolab.Types

-- assume CSV format of each row is:
-- expId, plate, measurement_type, timestamp, column, row, label, value
loadExpData :: FilePath -> IO ExpData
loadExpData filename = do
    file_data' <- parseFromFile csvFile filename
    let file_data = head . rights . return $ file_data'
    let ms = map readExpLine $ file_data
    return . createExpData $ ms

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

linesDataToCSV :: PlotLinesData -> String
linesDataToCSV = genCsvFile . concatMap linesData . M.toList

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

gridDataToCSV :: PlotGridData -> String
gridDataToCSV = genCsvFile . concatMap pointsData . M.toList
	    
