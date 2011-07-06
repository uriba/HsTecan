-- convert old style csv to new style csv.
-- input - two files - old-style csv + plate description csv
-- output - new style csv.
import Text.ParserCombinators.Parsec
import Data.CSV
import Data.Maybe
import Data.List
import Data.Either
import Data.Map ((!), Map)
import qualified Data.Map as M


-- Some types to make life easier...
type ExpId = String
type PlateId = Int
data Well = Well { wRow :: Char , wColumn :: Int } deriving (Eq, Show, Read, Ord)
data ColonyId = ColonyId { cExp :: ExpId, cPlate :: PlateId, cWell :: Well } deriving (Eq, Show, Ord)
data OldMeasurement = OldMeasurement { mPlateId :: PlateId, mWell :: Well, mTime :: Int , mType :: String, mVal :: Double } deriving (Eq, Show)

wPartialColumn :: Int -> (Char,Char) -> [Well]
wPartialColumn col (a,b) = zipWith Well [a..b] . repeat $ col

columnWells :: Int -> [Well]
columnWells col = wPartialColumn col ('a','h')

allWells96 = concatMap columnWells [1..12]

wellFromStr :: String -> Well
wellFromStr s = Well { wRow = ['a'..'h'] !! fst well_ind , wColumn = snd well_ind + 1 }
    where well_ind = read s :: (Int, Int)

wellToTouple :: Well -> (String,String)
wellToTouple (Well r c) = (show . fromJust . findIndex (== r) $ ['a'..'h'],show $ c-1)

maxMes = 70000

readExpLine :: [String] -> OldMeasurement
readExpLine m = OldMeasurement { 
		    mPlateId = 0, --read $ m !! 0, 
		    mWell = wellFromStr (m !! 3),
		    mTime = read $ m !! 2,
		    mType = m !! 1,
		    mVal = if m !! 4 == "nan" then maxMes else read $ m !! 4
		}

loadPlateExpData :: FilePath -> IO [OldMeasurement]
loadPlateExpData filename = do
    file_data' <- parseFromFile csvFile filename
    let file_data = head . rights . return $ file_data'
    return . map readExpLine . tail $ file_data -- first entry is the column description and cannot be parsed

data NewMeasurment = NewMeasurment {mExpId :: String, mLabel :: String, mOldMes :: OldMeasurement}
type Description = Map (PlateId,Well) String

defaultDescription :: Description
defaultDescription = M.fromList [((0, x),new_label x) | x <- allWells96]
    where
	new_label (Well a b) = a:(show b)
 
showNewMes :: NewMeasurment -> [String]
showNewMes nm = [
		    mExpId nm,
		    "0",
		    mType . mOldMes $ nm,
		    show . mTime . mOldMes $ nm,
		    fst . wellToTouple . mWell . mOldMes $ nm,
		    snd . wellToTouple . mWell . mOldMes $ nm,
		    mLabel nm,
		    show . mVal . mOldMes $ nm
		]

convertMes :: Description -> OldMeasurement -> NewMeasurment
convertMes d om = NewMeasurment { mExpId = "Converted", mLabel = d ! (0,mWell om), mOldMes = om }

writeNewData :: FilePath -> [NewMeasurment] -> IO()
writeNewData fn nm = do
	let file_data = genCsvFile . map showNewMes $ nm
	writeFile fn file_data
