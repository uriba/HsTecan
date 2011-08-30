{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
import Yesod
import Yesod.Helpers.Static
import Database.HDBC.MySQL
import Database.HDBC
import Data.ByteString.UTF8 (toString)
import Data.DateTime (fromSeconds)
import Data.Function (on)
import Data.List (nub, find, sort)
import Data.Maybe (isJust, fromJust)
import System.FilePath (makeValid, (<.>))
import Math.Combinatorics.Graph (combinationsOf)
import RoboLib (Measurement(..), Well(..), wellFromInts, createExpData, ExpData, plotData, mesData, expMesTypes, ExpId, MType, mesToOdData, plotIntensityGrid, smoothAll, bFiltS)
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Applicative ((<$>))

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
	mWell = well,
	mLabel = concat ["(", [wRow well],",",show . wColumn $ well,")"], 
	mVal = val v
    }
        where
            val (SqlDouble x) = if x == 0 then maxVal else x
            val SqlNull = maxVal
            well = (wellFromInts `on` fromIntegral) row col

data GraphDesc = GraphDesc {gdExp :: ExpId, gdPlate :: Plate, gdMesType :: MType, gdExpDesc :: Maybe String, gdPlateDesc :: Maybe String} deriving (Show)

data ExpDesc = ExpDesc {edExp :: ExpId, edDesc :: String} deriving (Show)
data PlateDesc = PlateDesc {pdExp :: ExpId, pdPlate :: Int, pdDesc :: String} deriving (Show)

dbToExpDesc :: [SqlValue] -> ExpDesc
dbToExpDesc [SqlByteString exp_id, SqlByteString desc] = 
    ExpDesc {
        edExp = toString exp_id,
        edDesc = toString desc
    }

dbToPlateDesc :: [SqlValue] -> PlateDesc
dbToPlateDesc [SqlByteString exp_id, SqlInt32 p, SqlByteString desc] = 
    PlateDesc {
        pdExp = toString exp_id,
        pdPlate = fromIntegral p,
        pdDesc = toString desc
    }

dbToGraphDesc :: [ExpDesc] -> [PlateDesc] -> [SqlValue] -> GraphDesc
dbToGraphDesc exp_descs plate_descs [SqlByteString exp_id, SqlInt32 p, SqlByteString mt] =
    GraphDesc {
	gdExp = toString exp_id,
	gdPlate = show p,
	gdMesType = toString mt,
    gdExpDesc = fmap edDesc . find (\x -> edExp x == toString exp_id) $ exp_descs,
    gdPlateDesc = fmap pdDesc . find (\x -> pdExp x == toString exp_id && pdPlate x == fromIntegral p) $ plate_descs
    }

loadExpDataDB :: ExpId -> Int -> IO ExpData
loadExpDataDB exp_id p = do
    conn <- connectMySQL $ MySQLConnectInfo hostName userName password dbName port unixSocket
    sql_vals <- quickQuery conn "SELECT * from tecan_readings where exp_id = ? AND plate = ?" [toSql exp_id, toSql p]
    return . createExpData . filter ((==) p . mPlate) . map dbToMes $ sql_vals

loadExps :: IO [GraphDesc]
loadExps = do
    conn <- connectMySQL $ MySQLConnectInfo hostName userName password dbName port unixSocket
    sql_vals <- quickQuery' conn "SELECT DISTINCT exp_id,plate,reading_label FROM tecan_readings" []
    db_exp_descs <-  quickQuery' conn "SELECT * FROM tecan_experiments" []
    let exp_descs = map dbToExpDesc db_exp_descs
    db_plate_descs <-  quickQuery' conn "SELECT * FROM tecan_plates" []
    let plate_descs = map dbToPlateDesc db_plate_descs
    return . map (dbToGraphDesc exp_descs plate_descs) $ sql_vals

data RoboSite = RoboSite
type Plate = String
type GraphType = String

mkYesod "RoboSite" [$parseRoutes|
/RoboSite HomeR GET
/RoboSite/update/#ExpId/ UpdateExpDesc GET
/RoboSite/update/#ExpId/#Plate/ UpdatePlateDesc GET
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
    let sms = smoothAll bFiltS ed
    let pd = mesToOdData sms t Nothing
    let	ofn = pngFileName fn
    plotData t pd (Just ofn)

plotGridApp :: ExpData -> (String,String) -> FilePath -> IO ()
plotGridApp ed axes fn = do
    let ofn = pngFileName fn
    let sms = smoothAll bFiltS ed
    plotIntensityGrid sms axes (logBase 10, logBase 10) (Just ofn)

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

updatePlateLabel :: ExpId -> Int -> String -> IO ()
updatePlateLabel eid p l = do
    conn <- connectMySQL $ MySQLConnectInfo hostName userName password dbName port unixSocket
    _ <- run conn "INSERT INTO tecan_plates (exp_id,plate,description) VALUES (?,?,?) ON DUPLICATE KEY UPDATE description = ?" [toSql eid, toSql p, toSql l, toSql l]
    return ()

updateExpLabel :: ExpId -> String -> IO ()
updateExpLabel eid l = do
    conn <- connectMySQL $ MySQLConnectInfo hostName userName password dbName port unixSocket
    _ <- run conn "INSERT INTO tecan_experiments (exp_id,description) VALUES (?,?) ON DUPLICATE KEY UPDATE description = ?" [toSql eid, toSql l, toSql l]
    return ()

data Params = Params {label :: T.Text}

descFormlet :: Formlet s m Params
descFormlet mdesc = fieldsToTable $ Params <$> stringField "Label" (fmap label mdesc)

getUpdatePlateDesc :: ExpId -> Plate -> GHandler RoboSite RoboSite RepHtml
getUpdatePlateDesc eid p = updateDesc eid (Just p)

getUpdateExpDesc :: ExpId -> GHandler RoboSite RoboSite RepHtml
getUpdateExpDesc eid = updateDesc eid Nothing

updateDesc :: ExpId -> Maybe Plate -> GHandler RoboSite RoboSite RepHtml
updateDesc eid mp = do
    let m_label = Just . Params . T.pack $ "default"
    (res, form, enctype) <- runFormGet $ descFormlet m_label
    output <-
        case res of
            FormMissing -> return . T.pack $ "Enter new label"
            FormFailure _ -> return . T.pack $ "Please correct the errors below."
            FormSuccess (Params label) -> do
                if isJust mp
                    then
                        liftIO . updatePlateLabel eid (read . fromJust $ mp) . T.unpack $ label
                    else
                        liftIO . updateExpLabel eid . T.unpack $ label
                return . T.pack $ "Label updated"
    defaultLayout [$hamlet|
<p>Enter new label
<form enctype="#{enctype}">
    <table>
        ^{form}
        <tr>
            <td colspan="2">
                <input type="submit" value="Update">
|]

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
            \ #
            <a href=@{UpdateExpDesc (fst exp)}>
                [Update label]
            <div ##{fst exp} style="display: none;">
                <ul>
                   $forall plate <- plates (fst exp) disp_data
                       <li>
                            <a href="#" onclick="toggleDisplay('#{fst exp}#{fst plate}');">
                                $maybe desc <- snd plate
                                    Plate #{fst plate}: #{desc}
                                $nothing
                                    Plate #{fst plate}
                            \ #
                            <a href=@{UpdatePlateDesc (fst exp) (fst plate)}>
                                [Update label]
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
