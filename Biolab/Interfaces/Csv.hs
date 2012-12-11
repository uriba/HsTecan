module Biolab.Interfaces.Csv (
    loadExpData,
    processedDataToCSV,
    correlationDataToCSV,
    measureDataToCSV,
)
where
import Text.ParserCombinators.Parsec
import Text.CSV
import Data.Either (rights)
import Data.DateTime (fromSeconds, toSqlString)
import Data.Function (on)
import Data.Map (Map)
import Data.Either.Unwrap (fromRight)
import qualified Data.Map as M
import qualified Data.Vector.Generic as G
import Biolab.Constants (maxMes)
import Biolab.Types
import Biolab.Utils.Vector (Series)
import Biolab.Measurement (wellFromInts)
import Biolab.ExpData (createExpData)

-- assume CSV format of each row is:
-- expId, plate, measurement_type, timestamp, column, row, label, value
loadExpData :: FilePath -> IO ExpData
loadExpData filename = do
    file_data' <- parseCSVFromFile filename
    let file_data = fromRight file_data'
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
processedDataToCSV :: ProcessedData -> String
processedDataToCSV = exportedDataToCSV . ldMap (concatMap (\(x,y) -> [show x, show y]) . G.toList)

correlationDataToCSV :: CorrelationData -> String
correlationDataToCSV = exportedDataToCSV . ldMap (\(x,y) -> [show x, show y])

measureDataToCSV :: MeasureData -> String
measureDataToCSV = exportedDataToCSV . ldMap (return . show)

toLines :: (ColonyId, [String]) -> [String]
toLines (cid,xs) = [
    cExp cid,
    show . cPlate $ cid,
    [wRow . cWell $ cid],
    show . wColumn . cWell $ cid
    ] ++ xs

labelLines :: (String, Map ColonyId [String]) -> [[String]]
labelLines (label,lines) = zipWith (:) (repeat label) . map toLines . M.toList $ lines

exportedDataToCSV :: ExportedData -> String
exportedDataToCSV = printCSV . concatMap labelLines . M.toList
