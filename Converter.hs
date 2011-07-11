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

oldToNew :: (String,[Well]) -> [((PlateId, Well),String)]
oldToNew (label,wells) = zip (zip (repeat 0) wells) . repeat $ label

mapOldToNew = M.fromList  . concatMap oldToNew 

bestOfBest2Desc :: Description
bestOfBest2Desc = mapOldToNew [
		("mA",[Well 'a' 11, Well 'a' 12]),
		("mB",[Well 'b' 11, Well 'b' 12]),
		("mC",[Well 'c' 11, Well 'c' 12]),
		("mD",[Well 'd' 11, Well 'd' 12]),
		("yA",[Well 'e' 12]),
		("yB",[Well 'f' 12]),
		("yC",[Well 'g' 12]),
		("yD",[Well 'h' 12]),
		("A?",[Well 'e' 11]),
		("AD", [Well 'a' 1, Well 'a' 3, Well 'b' 5, Well 'f' 11]),
		("CA", [Well 'd' 3, Well 'b' 2, Well 'h' 11]),
		("DA", [Well 'g' 5, Well 'd' 1]),
		("DC", [Well 'f' 4, Well 'c' 6, Well 'd' 4]),
		("DD", [Well 'f' 5, Well 'g' 3, Well 'h' 1]),
		("CD", [Well 'h' 4, Well 'b' 3, Well 'b' 4, Well 'd' 5,Well 'e' 2]),
		("CC", [Well 'e' 1]),
		("CFP-A", [Well 'a' 10]),
		("Wild Type", [Well 'e' 6, Well 'f' 6, Well 'g' 6]),
		("Unmapped", (concatMap (zipWith Well ['a'..'h'] . repeat) [7..10] \\ [Well 'a' 10]) ++ [Well 'g' 11, Well 'a' 2,Well 'a' 5,Well 'a' 6,Well 'b' 1,Well 'b' 6,Well 'c' 1,Well 'c' 2,Well 'c' 3,Well 'c' 4,Well 'c' 5,Well 'd' 2,Well 'd' 6,Well 'e' 3,Well 'e' 5,Well 'f' 2,Well 'f' 3,Well 'g' 1,Well 'g' 2,Well 'g' 4,Well 'h' 3,Well 'h' 5,Well 'h' 6])]

pairsC :: Description
pairsC = mapOldToNew [
    ("AC", zipWith Well ['a'..'g'] . repeat $ 1),
    -- ("A2", zipWith Well ['a'..'g'] . repeat $ 2),
    ("BC", zipWith Well ['a'..'g'] . repeat $ 3),
    -- ("B2", zipWith Well ['a'..'g'] . repeat $ 4),
    ("CC", zipWith Well ['a'..'g'] . repeat $ 5),
    -- ("C2", zipWith Well ['a'..'g'] . repeat $ 6),
    ("DC", zipWith Well ['a'..'g'] . repeat $ 7),
    -- ("D2", zipWith Well ['a'..'g'] . repeat $ 8),
    ("EC", zipWith Well ['a'..'g'] . repeat $ 9),
    -- ("E2", zipWith Well ['a'..'g'] . repeat $ 10),
    -- ("Z1", zipWith Well ['a'..'g'] . repeat $ 11),
    ("yA", zipWith Well (repeat 'h') [1,2]),
    ("yB", zipWith Well (repeat 'h') [3,4]),
    ("yC", zipWith Well (repeat 'h') [5,6]),
    ("yD", zipWith Well (repeat 'h') [7,8]),
    ("yE", zipWith Well (repeat 'h') [9,10]),
    ("yZ", zipWith Well (repeat 'h') [11,12]),
    ("mC", zipWith Well ['a','b'] . repeat $ 12),
    ("mD", zipWith Well ['c','d'] . repeat $ 12),
    ("Wild Type", zipWith Well ['e'] . repeat $ 12),
    ("Just Media", [Well 'g' 12])]

pairsD :: Description
pairsD = mapOldToNew [
    ("AD", zipWith Well ['a'..'g'] . repeat $ 1),
    -- ("A2", zipWith Well ['a'..'g'] . repeat $ 2),
    ("BD", zipWith Well ['a'..'g'] . repeat $ 3),
    -- ("B2", zipWith Well ['a'..'g'] . repeat $ 4),
    -- ("D1", zipWith Well ['a'..'g'] . repeat $ 5),
    ("DD", zipWith Well ['a'..'g'] . repeat $ 6),
    -- ("E1", zipWith Well ['a'..'g'] . repeat $ 7),
    ("ED", zipWith Well ['a'..'g'] . repeat $ 8),
    ("ZD", zipWith Well ['a'..'g'] . repeat $ 9),
    -- ("Z2", zipWith Well ['a'..'g'] . repeat $ 10),
    ("yA", zipWith Well (repeat 'h') [1,2]),
    ("yB", zipWith Well (repeat 'h') [3,4]),
    ("yC", zipWith Well (repeat 'h') [5,6]),
    ("yD", zipWith Well (repeat 'h') [7,8]),
    ("yE", zipWith Well (repeat 'h') [9,10]),
    ("yZ", zipWith Well (repeat 'h') [11,12]),
    ("mC", zipWith Well ['a','b'] . repeat $ 12),
    ("mD", zipWith Well ['c','d'] . repeat $ 12),
    ("Wild Type", zipWith Well ['e','f'] . repeat $ 12),
    ("Just Media", [Well 'g' 12])]
 
plateRGBNewSingleDH5a1 = mapOldToNew [
		("mch-L1-2m", rowWells 'a' ++ rowWells 'b'),
		("mch-L1-ptac", rowWells 'c' ++ rowWells 'd'),
		("mch-L2-ptac", rowWells 'e' ++ rowWells 'f'),
		("yfp-L1-ptac", rowWells 'g' ++ rowWells 'h')
	    ]

plateRGBNewSingleDH5a2 = mapOldToNew [
		("yfp-L2-ptac", rowWells 'a' ++ rowWells 'b'),
		("yfp-L1-m2", rowWells 'c'),
--		("cfp-L1-ptac", rowWells 'd'),
--		("cfp-L2-ptac", rowWells 'e'),
--		("cfp-L1-m2", rowWells 'f'),
		("control YFP a-z", wPartialRow 'g' (1,6)),
		("control mCherry a-z", wPartialRow 'g' (7,12)),
--		("control CFP a-z", wPartialRow 'h' (1,6)),
		("control wt", wPartialRow 'h' (7,9))
			    ]

plateRGBNewSingleK121 = mapOldToNew [
		("mch-L1-2m", rowWells 'a' ++ rowWells 'h'),
		("mch-L1-ptac", rowWells 'b' ++ rowWells 'c'),
		("mch-L2-ptac", rowWells 'd' ++ rowWells 'e'),
		("yfp-L1-2m", rowWells 'f' ++ rowWells 'g')
	    ]
plateRGBNewSingleK122 =  mapOldToNew [
		("yfp-L1-ptac", rowWells 'a' ++ rowWells 'b'),
		("yfp-L2-ptac", rowWells 'c'),
--		("cfp-L1-ptac", rowWells 'd'),
--		("cfp-L2-ptac", rowWells 'e'),
--		("cfp-L1-m2", rowWells 'f'),
		("control YFP a-z", wPartialRow 'g' (1,6)),
		("control mCherry a-z", wPartialRow 'g' (7,12)),
--		("control CFP a-z", wPartialRow 'h' (1,6)),
		("control wt", wPartialRow 'h' (7,9))
			    ]
-- Plate utils...

wPartialRow :: Char -> (Int,Int) -> [Well]
wPartialRow row (a,b)= zipWith Well (repeat row) [a..b]

rowWells :: Char -> [Well]
rowWells row = wPartialRow row (1,12)
convertMes :: Description -> OldMeasurement -> Maybe NewMeasurment
convertMes d om = do
    ml <- M.lookup (0,mWell om) d
    return NewMeasurment { mExpId = "Converted", mLabel = ml, mOldMes = om }

writeNewData :: FilePath -> [NewMeasurment] -> IO()
writeNewData fn nm = do
	let file_data = genCsvFile . map showNewMes $ nm
	writeFile fn file_data
