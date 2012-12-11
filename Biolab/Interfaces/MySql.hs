module Biolab.Interfaces.MySql (
    DbReadable(..),
    ExpDesc(..),
    PlateDesc(..),
    DbMeasurement(..),
    WellDesc(..),
    SelectCriteria(..),
    readTable,
    dbConnectInfo,
    mesFromDB,
    loadExpDataDB,
    fromNullString
    )
where
import Database.HDBC.MySQL
import Database.HDBC
import Data.ByteString.UTF8 (toString)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.DateTime (fromSeconds, DateTime)
import Biolab.Types (Well(..), Measurement(..), ExpData(..), MType, ExpId, wellStr)
import Biolab.Measurement (wellFromInts)
import Biolab.ExpData (createExpData)
import Data.List (find)
import Control.Monad.Error (runErrorT)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Either.Unwrap (fromRight)
import Data.ConfigFile (emptyCP, readfile, get)

-- consider adding table names to configuration file as well.
dbConnectInfo :: FilePath -> IO MySQLConnectInfo
dbConnectInfo cf = do
    rv <- runErrorT $
        do
            cp <- join $ liftIO $ readfile emptyCP cf
            let sect = "MYSQL"
            host <- get cp sect "host"
            user <- get cp sect "user"
            passwd <- get cp sect "password"
            dbname <- get cp sect "dbname"
            port <- get cp sect "port"
            unixsock <- get cp sect "unixsocket"
            return $ MySQLConnectInfo {
                mysqlHost = host,
                mysqlUser = user,
                mysqlPassword = passwd,
                mysqlDatabase = dbname,
                mysqlPort = port,
                mysqlUnixSocket = unixsock,
                mysqlGroup = Nothing
            }
    return $ fromRight rv

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

data WellDesc = WellDesc {wdExp :: ExpId, wdPlate :: Int, wdWell :: Well, wdDesc :: String} deriving (Show, Eq)

instance DbReadable WellDesc where
    dbRead [SqlByteString exp_id, SqlInt32 p, SqlInt32 row, SqlInt32 col, SqlByteString desc] = 
        WellDesc {
            wdExp = toString exp_id,
            wdPlate = fromIntegral p,
            wdWell = (wellFromInts `on` fromIntegral) row col,
            wdDesc = toString desc
        }

data PlateDesc = PlateDesc {pdExp :: ExpId, pdPlate :: Int, pdDesc :: String, pdOwner :: Maybe String, pdProject :: Maybe String} deriving (Show)

fromNullString :: SqlValue -> Maybe String
fromNullString SqlNull = Nothing
fromNullString (SqlByteString s) = Just . toString $ s

instance DbReadable PlateDesc where
    dbRead [SqlByteString exp_id, SqlInt32 p, SqlByteString desc, owner, project] = --SqlByteString owner, SqlByteString project] = 
        PlateDesc {
            pdExp = toString exp_id,
            pdPlate = fromIntegral p,
            pdDesc = toString desc,
            pdOwner = fromNullString owner,
            pdProject = fromNullString project
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

readTable :: (DbReadable a) => MySQLConnectInfo -> String -> Maybe SelectCriteria -> IO [a]
readTable db_conf t_name msc = do
    conn <- connectMySQL db_conf
    let where_clause = fromMaybe "" . fmap scWhere $ msc
    let where_params = fromMaybe [] . fmap scVals $ msc
    entries <-  quickQuery' conn ("SELECT * FROM " ++ t_name ++ " " ++ where_clause) where_params
    return . map dbRead $ entries

loadExpDataDB :: FilePath -> ExpId -> Int -> IO ExpData
loadExpDataDB cf exp_id p = do
    db_conf <- dbConnectInfo cf
    readings <- readTable db_conf "tecan_readings" (Just $ SelectCriteria "where exp_id = ? AND plate = ?" [toSql exp_id, toSql p])
    well_labels <- readTable db_conf "tecan_labels" (Just $ SelectCriteria "where exp_id = ? AND plate = ?" [toSql exp_id, toSql p])
    return . createExpData . mesFromDB well_labels $ readings

mesFromDB :: [WellDesc] -> [DbMeasurement] -> [Measurement]
mesFromDB wds dbms = [ to_mes m | m <- dbms]
    where
        to_mes m = Measurement {
            mExpDesc = dbmExpDesc m,
            mPlate = dbmPlate m,
            mType = dbmType m,
            mTime = dbmTime m,
            mWell = dbmWell m,
            mLabel = fromMaybe (wellStr $ dbmWell m) . fmap wdDesc . find ((==) (dbmWell m) . wdWell) $ wds,
            mVal = dbmVal m
        }
