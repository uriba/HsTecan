module RoboCSV (
    loadExpData,
)
where
import Text.ParserCombinators.Parsec
import Data.CSV
import Data.Either (rights)
import RoboLib (Measurement(..), ExpData(..), ColonyId, colonyId, wellFromInts, createExpData)
import Data.DateTime (fromSeconds, toSqlString)
import Data.List (nub)
import Data.Function (on)
import RoboUtils

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

