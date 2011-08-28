{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
import Yesod
import Yesod.Helpers.Static
import Database.HDBC.MySQL
import Database.HDBC
import Data.ByteString.UTF8 (toString)
import Data.DateTime (fromSeconds)
import Data.Function (on)
import Data.List (nub, find, sort)
import System.FilePath (makeValid, (<.>))
import Math.Combinatorics.Graph (combinationsOf)
import RoboLib (Measurement(..), Well(..), wellFromInts, createExpData, ExpData, plotData, mesData, expMesTypes, ExpId, MType, mesToOdData, plotIntensityGrid)

hostName = "132.77.80.238"
userName = "ronm"
password = "a1a1a1"
dbName = "tecan"
port = 3306
unixSocket = "/tmp/mysql.sock"

maxVal = 70000

dbToMes :: [SqlValue] -> Measurement
dbToMes [SqlByteString exp_id, SqlInt32 plate_num, SqlByteString mt, SqlInt32 row, SqlInt32 col, SqlInt32 timestamp, v] = Measurement {
	mExpDesc = toString exp_id,
	mPlate = fromIntegral plate_num,
	mTime = fromSeconds . fromIntegral $ timestamp,
	mType = toString mt,
	mWell = (wellFromInts `on` fromIntegral) row col,
	mLabel = show row ++ show col,
	mVal = val v
    }
        where
            val (SqlDouble x) = if x == 0 then maxVal else x
            val SqlNull = maxVal

data GraphDesc = GraphDesc {gdExp :: ExpId, gdPlate :: Plate, gdMesType :: MType, gdExpDesc :: Maybe String, gdPlateDesc :: Maybe String} deriving (Show)

data ExpDesc = ExpDesc {edExp :: ExpId, edPlate :: Plate, edDesc :: String} deriving (Show)

dbToExpDesc :: [SqlValue] -> ExpDesc
dbToExpDesc [SqlByteString exp_id, SqlInt32 p, SqlByteString desc] = 
    ExpDesc {
        edExp = toString exp_id,
        edPlate = show p,
        edDesc = toString desc
    }

dbToGraphDesc :: [ExpDesc] -> [SqlValue] -> GraphDesc
dbToGraphDesc descs [SqlByteString exp_id, SqlInt32 p, SqlByteString mt] =
    GraphDesc {
	gdExp = toString exp_id,
	gdPlate = show p,
	gdMesType = toString mt,
    gdExpDesc = fmap edDesc . find (\x -> edExp x == toString exp_id && edPlate x == "-1") $ descs,
    gdPlateDesc = fmap edDesc . find (\x -> edExp x == toString exp_id && edPlate x == show p) $ descs
    }

loadExpDataDB :: ExpId -> Int -> IO ExpData
loadExpDataDB exp_id p = do
    conn <- connectMySQL $ MySQLConnectInfo hostName userName password dbName port unixSocket
    sql_vals <- quickQuery conn "SELECT * from tecan_readings where exp_id = ?" [toSql exp_id]
    return . createExpData . filter ((==) p . mPlate) . map dbToMes $ sql_vals

loadExps :: IO [GraphDesc]
loadExps = do
    conn <- connectMySQL $ MySQLConnectInfo hostName userName password dbName port unixSocket
    sql_vals <- quickQuery' conn "SELECT DISTINCT exp_id,plate,reading_label FROM tecan_readings" []
    putStrLn . show . head $ sql_vals
    exp_descs <-  quickQuery' conn "SELECT * FROM tecan_experiments" []
    putStrLn . show . head $ exp_descs
    let descs = map dbToExpDesc exp_descs
    return . map (dbToGraphDesc descs) $ sql_vals

data RoboSite = RoboSite
type Plate = String
type GraphType = String

mkYesod "RoboSite" [$parseRoutes|
/RoboSite HomeR GET
/RoboSite/graph/#ExpId/#Plate/#GraphType/Read ReadGraph GET
/RoboSite/graph/#ExpId/#Plate/#GraphType/Exp ExpLevelGraph GET
/RoboSite/graph/#ExpId/#Plate/#GraphType/#GraphType/Exp GridGraph GET
|]

instance Yesod RoboSite where
    approot _ = ""

pngFileName :: FilePath -> String
pngFileName fn = fn ++ ".png"

plotMesApp :: ExpData -> FilePath -> String -> IO ()
plotMesApp ed fn t = do
    let pd = mesData ed t
    let	ofn = pngFileName fn
    plotData t pd (Just ofn)

plotMesToODApp :: ExpData -> FilePath -> String -> IO ()
plotMesToODApp ed fn t = do
    let pd = mesToOdData ed t Nothing
    let	ofn = pngFileName fn
    plotData t pd (Just ofn)

plotGridApp :: ExpData -> (String,String) -> FilePath -> IO ()
plotGridApp ed axes fn = do
    let ofn = pngFileName fn
    plotIntensityGrid ed axes (logBase 10, logBase 10) (Just ofn)

getGridGraph :: ExpId -> Plate -> GraphType -> GraphType -> GHandler RoboSite RoboSite RepHtml
getGridGraph exp plate x y = do
    exp_data <- liftIO $ loadExpDataDB exp (read plate)
    let fn = makeValid (exp <.> plate <.> x <.> y <.> "grid")
    liftIO $ putStrLn ("generating: " ++ pngFileName fn)
    liftIO $ plotGridApp exp_data (x,y) fn
    sendFile typePng $ pngFileName fn

getExpLevelGraph :: ExpId -> Plate -> GraphType -> GHandler RoboSite RoboSite RepHtml
getExpLevelGraph exp plate graph = do
    exp_data <- liftIO $ loadExpDataDB exp (read plate)
    let fn = makeValid (exp <.> plate <.> graph <.> "expLevel")
    liftIO $ putStrLn ("generating: " ++ pngFileName fn)
    liftIO $ plotMesToODApp exp_data fn graph
    sendFile typePng $ pngFileName fn

getReadGraph :: ExpId -> Plate -> GraphType -> GHandler RoboSite RoboSite RepHtml
getReadGraph exp plate graph = do
    exp_data <- liftIO $ loadExpDataDB exp (read plate)
    let fn = makeValid (exp <.> graph <.> plate)
    liftIO $ putStrLn ("generating: " ++ pngFileName fn)
    liftIO $ plotMesApp exp_data fn graph
    sendFile typePng $ pngFileName fn

plates :: ExpId -> [GraphDesc] -> [(Plate,Maybe String)]
plates eid gds = nub [(gdPlate x, gdPlateDesc x) | x <- gds, gdExp x == eid]

readings :: ExpId -> Plate -> [GraphDesc] -> [MType]
readings eid p = nub . map gdMesType . filter (\x -> gdExp x == eid && gdPlate x == p)

expLevels :: ExpId -> Plate -> [GraphDesc] -> [MType]
expLevels e p = filter (/= "OD600") . readings e p

grids :: ExpId -> Plate -> [GraphDesc] -> [(MType,MType)]
grids e p gd = zip (map head all_pairs) (map last all_pairs)
    where
        all_pairs = combinationsOf 2 . expLevels e p $ gd

getHomeR :: GHandler RoboSite RoboSite RepHtml
getHomeR = do
    disp_data <- liftIO loadExps
    let exp_descs =  reverse . sort . nub $ [ (gdExp x, gdExpDesc x) | x <- disp_data]
    let exp_ids = nub . map gdExp $ disp_data
    defaultLayout $ do
        setTitle "Robosite"
        addHamlet [$hamlet|
<h1> Welcome to the Robosite
<h2> experiments list:
<ul>
    $forall exp <- exp_descs
        <li>
            <a href="#" onclick="toggleDisplay('#{fst exp}');">
                $maybe desc <- snd exp
                    #{fst exp}: #{desc}
                $nothing
                    #{fst exp}
            <div ##{fst exp} style="display: none;">
                <ul>
                   $forall plate <- plates (fst exp) disp_data
                       <li>
                            <a href="#" onclick="toggleDisplay('#{fst exp}#{fst plate}');">
                                $maybe desc <- snd plate
                                    Plate #{fst plate}: #{desc}
                                $nothing
                                    Plate #{fst plate}
                            <div ##{fst exp}#{fst plate} style="display: none;">
                                <ul>
                                    <li>
                                        Raw sensor readings
                                        <ul>
                                            $forall mtype <- readings (fst exp) (fst plate) disp_data
                                                <li>
                                                    <a href=@{ReadGraph (fst exp) (fst plate) mtype}>
                                                        #{mtype}
                                    <li>
                                        Expression levels
                                        <ul>
                                            $forall mtype <- expLevels (fst exp) (fst plate) disp_data
                                                <li>
                                                    <a href=@{ExpLevelGraph (fst exp) (fst plate) mtype}>
                                                        #{mtype}
                                    <li>
                                        Grids
                                        <ul>
                                            $forall grid <- grids (fst exp) (fst plate) disp_data
                                                <li>
                                                    <a href=@{GridGraph (fst exp) (fst plate) (fst grid) (snd grid)}>
                                                        #{fst grid},#{snd grid}
|]
        addJulius [$julius|
function toggleDisplay(element_name) {
  document.getElementById(element_name).style.visibility = "visible";
  if(document.getElementById(element_name).style.display == "none" ) {
    document.getElementById(element_name).style.display = "";
  }
  else {
    document.getElementById(element_name).style.display = "none";
  }
}
|]

main = warpDebug 3000 RoboSite
