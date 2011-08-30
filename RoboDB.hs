module RoboDB (
    DbReadable(..),
    ExpDesc(..),
    PlateDesc(..),
    DbMeasurement(..),
    WellDesc(..),
    SelectCriteria(..),
    readTable,
    dbConnectInfo
    )
where
import Database.HDBC.MySQL
import Database.HDBC
import Data.ByteString.UTF8 (toString)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.DateTime (fromSeconds, DateTime)
import RoboLib (ExpId, Well(..), wellFromInts, Measurement(..), MType)

dbConnectInfo = MySQLConnectInfo {
    mysqlHost = "132.77.80.238",
    mysqlUser = "ronm",
    mysqlPassword = "a1a1a1",
    mysqlDatabase = "tecan",
    mysqlPort = 3306,
    mysqlUnixSocket = "/tmp/mysql.sock"
}

maxVal = 70000

data SelectCriteria = SelectCriteria {scWhere :: String, scVals :: [SqlValue]}
    deriving (Show)

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

data DbMeasurement = DbMeasurement { dbmExpDesc :: ExpId, dbmPlate :: Int, dbmTime :: DateTime, dbmType :: MType, dbmWell :: Well, dbmVal :: Double } deriving (Eq, Show)

instance DbReadable DbMeasurement where
    dbRead [SqlByteString exp_id, SqlInt32 plate_num, SqlByteString mt, SqlInt32 row, SqlInt32 col, SqlInt32 timestamp, v] =
        DbMeasurement {
            dbmExpDesc = toString exp_id,
            dbmPlate = fromIntegral plate_num,
            dbmTime = fromSeconds . fromIntegral $ timestamp,
            dbmType = toString mt,
            dbmWell = well,
            dbmVal = val v
            }
                where
                    val (SqlDouble x) = if x == 0 then maxVal else x
                    val SqlNull = maxVal
                    well = (wellFromInts `on` fromIntegral) row col

readTable :: (DbReadable a) => String -> Maybe SelectCriteria -> IO [a]
readTable t_name msc = do
    conn <- connectMySQL dbConnectInfo
    let where_clause = fromMaybe "" . fmap scWhere $ msc
    let where_params = fromMaybe [] . fmap scVals $ msc
    entries <-  quickQuery' conn ("SELECT * FROM " ++ t_name ++ " " ++ where_clause) where_params
    return . map dbRead $ entries

