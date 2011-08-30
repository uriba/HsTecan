module RoboDB (
    DbReadable(..),
    ExpDesc(..),
    PlateDesc(..),
    WellDesc(..),
    readTable,
    dbConnectInfo
    )
where
import Database.HDBC.MySQL
import Database.HDBC
import Data.ByteString.UTF8 (toString)
import Data.Function (on)
import Data.DateTime (fromSeconds)
import RoboLib (ExpId, Well(..), wellFromInts, Measurement(..))

dbConnectInfo = MySQLConnectInfo {
    mysqlHost = "132.77.80.238",
    mysqlUser = "ronm",
    mysqlPassword = "a1a1a1",
    mysqlDatabase = "tecan",
    mysqlPort = 3306,
    mysqlUnixSocket = "/tmp/mysql.sock"
}

maxVal = 70000

class DbReadable a where
    dbRead :: [SqlValue] -> a

data ExpDesc = ExpDesc {edExp :: ExpId, edDesc :: String} deriving (Show)
instance DbReadable ExpDesc where
    dbRead [SqlByteString exp_id, SqlByteString desc] = 
        ExpDesc {
            edExp = toString exp_id,
            edDesc = toString desc
        }

data WellDesc = WellDesc {wdExp :: ExpId, wdPlate :: Int, wdWell :: Well, wdDesc :: String} deriving (Show)
instance DbReadable WellDesc where
    dbRead [SqlByteString exp_id, SqlInt32 p, SqlInt32 row, SqlInt32 col, SqlByteString desc] = 
        WellDesc {
            wdExp = toString exp_id,
            wdPlate = fromIntegral p,
            wdWell = (wellFromInts `on` fromIntegral) row col,
            wdDesc = toString desc
        }

data PlateDesc = PlateDesc {pdExp :: ExpId, pdPlate :: Int, pdDesc :: String} deriving (Show)
instance DbReadable PlateDesc where
    dbRead [SqlByteString exp_id, SqlInt32 p, SqlByteString desc] = 
        PlateDesc {
            pdExp = toString exp_id,
            pdPlate = fromIntegral p,
            pdDesc = toString desc
        }

readTable :: (DbReadable a) => String -> IO [a]
readTable t_name = do
    conn <- connectMySQL dbConnectInfo
    entries <-  quickQuery' conn ("SELECT * FROM " ++ t_name) []
    return . map dbRead $ entries

instance DbReadable Measurement where
    dbRead [SqlByteString exp_id, SqlInt32 plate_num, SqlByteString mt, SqlInt32 row, SqlInt32 col, SqlInt32 timestamp, v] =
        Measurement {
            mExpDesc = toString exp_id,
            mPlate = fromIntegral plate_num,
            mTime = fromSeconds . fromIntegral $ timestamp,
            mType = toString mt,
            mWell = well,
            mLabel = concat ["(", [wRow well],",",show . wColumn $ well,")"], 
            mVal = val v
            }
                where
                    val (SqlDouble x) = if x == 0 then maxVal else x
                    val SqlNull = maxVal
                    well = (wellFromInts `on` fromIntegral) row col
