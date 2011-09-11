{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, TypeSynonymInstances, OverlappingInstances #-}
import Yesod
import Database.HDBC.MySQL
import Database.HDBC
import Data.ByteString.UTF8 (toString)
import Data.Function (on)
import Data.List (nub, find, sort)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.DateTime (toSeconds, DateTime)
import System.FilePath (makeValid, (<.>))
import Math.Combinatorics.Graph (combinationsOf)
import RoboDB (ExpDesc(..), PlateDesc(..), WellDesc(..), DbMeasurement (..), readTable, DbReadable(..), dbConnectInfo, SelectCriteria(..))
import RoboLib (Measurement(..), Well(..), wellFromInts, createExpData, ExpData, plotData, timedMesData, expMesTypes, ExpId, MType, mesToOdData, timedMesToOdData, plotIntensityGrid, smoothAll, bFiltS, intensityGridData, plotGridDataToStrings, plotLinesDataToStrings, Label, PlotGridData, ColonyId(..), wellStr, TimedPlotLinesData)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Text.JSON as J
import Control.Applicative ((<$>))
import Data.CSV (genCsvFile)
import HighChartsJson

data GraphDesc = GraphDesc {gdExp :: ExpId, gdPlate :: Plate, gdMesType :: MType, gdExpDesc :: Maybe String, gdPlateDesc :: Maybe String} deriving (Show)

dbToGraphDesc :: [ExpDesc] -> [PlateDesc] -> [SqlValue] -> GraphDesc
dbToGraphDesc exp_descs plate_descs [SqlByteString exp_id, SqlInt32 p, SqlByteString mt] =
    GraphDesc {
    gdExp = toString exp_id,
    gdPlate = show p,
    gdMesType = toString mt,
    gdExpDesc = fmap edDesc . find (\x -> edExp x == toString exp_id) $ exp_descs,
    gdPlateDesc = fmap pdDesc . find (\x -> pdExp x == toString exp_id && pdPlate x == fromIntegral p) $ plate_descs
    }

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

loadExpDataDB :: ExpId -> Int -> IO ExpData
loadExpDataDB exp_id p = do
    readings <- readTable "tecan_readings" (Just $ SelectCriteria "where exp_id = ? AND plate = ?" [toSql exp_id, toSql p])
    well_labels <- readTable "tecan_labels" (Just $ SelectCriteria "where exp_id = ? AND plate = ?" [toSql exp_id, toSql p])
    return . createExpData . mesFromDB well_labels $ readings

loadExps :: IO [GraphDesc]
loadExps = do
    conn <- connectMySQL dbConnectInfo
    sql_vals <- quickQuery' conn "SELECT DISTINCT exp_id,plate,reading_label FROM tecan_readings" []
    exp_descs <- readTable "tecan_experiments" Nothing
    plate_descs <- readTable "tecan_plates" Nothing
    return . map (dbToGraphDesc exp_descs plate_descs) $ sql_vals

data RoboSite = RoboSite
type Plate = String
type GraphType = String

mkYesod "RoboSite" [$parseRoutes|
/RoboSite HomeR GET
/RoboSite/update/#ExpId/ UpdateExpDesc GET
/RoboSite/update/#ExpId/#Plate/ UpdatePlateDesc GET
/RoboSite/graph/#ExpId/#Plate/#GraphType/Read ReadGraph GET
/RoboSite/data/#ExpId/#Plate/#GraphType/Read ReadGraphCSV GET
/RoboSite/graph/#ExpId/#Plate/#GraphType/Exp ExpLevelGraph GET
/RoboSite/data/#ExpId/#Plate/#GraphType/Exp ExpLevelCSV GET
/RoboSite/graph/#ExpId/#Plate/#GraphType/#GraphType/Exp GridGraph GET
/RoboSite/data/#ExpId/#Plate/#GraphType/#GraphType/Exp GridGraphCSV GET
|]

instance Yesod RoboSite where
    approot _ = ""

graphPage :: String -> String -> [(String,J.JSValue)] -> GHandler RoboSite RoboSite RepHtml
graphPage title div chart_json = do
    let json_data = J.encode . J.makeObj $ chart_json
    defaultLayout $ do
        setTitle "Graph"
        addJulius [$julius|
        </script>
        <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js"></script>
        <script type="text/javascript" src="/js/highcharts.js"></script> 
        <script type="text/javascript" src="/js/modules/exporting.js"></script> 
        
        <!-- 2. Add the JavaScript to initialize the chart on document ready --> 
        <script type="text/javascript"> 
            var chart;
            $(document).ready(function() {
                chart = new Highcharts.Chart(#{json_data});
            });
|]

        addHamlet [$hamlet|
<h1> #{title}
<div ##{div} style="width: 800px; height: 800px; margin: 0 auto">
|]

getGridGraphData :: ExpId -> Plate -> MType -> MType -> IO PlotGridData
getGridGraphData exp plate x y = do
    exp_data <- liftIO $ loadExpDataDB exp (read plate)
    let sms = smoothAll bFiltS exp_data
    return $ intensityGridData sms (x,y) (logBase 10, logBase 10)

getGridGraphCSV :: ExpId -> Plate -> MType -> MType -> GHandler RoboSite RoboSite RepHtml
getGridGraphCSV exp plate x y = do
    igd <- liftIO $ getGridGraphData exp plate x y
    let bytes = genCsvFile . plotGridDataToStrings $ igd
    sendResponse (typePlain, toContent bytes)

getGridGraph :: ExpId -> Plate -> MType -> MType -> GHandler RoboSite RoboSite RepHtml
getGridGraph exp plate x y = do
    igd <- liftIO $ getGridGraphData exp plate x y
    let div_obj = "container"
    let title = "Grid data of (" ++ x ++ ", " ++ y ++ ")"
    let subtitle = "Experiment: " ++ exp ++ ", Plate: " ++ plate
    let chart_json = [chartTitle title, chartSubtitle subtitle, chartXaxis x Nothing (Just (1.5,6)), chartYaxis y Nothing (Just (1.5,6)), gridChart div_obj, chartLegend] ++ gridChartSeries igd
    graphPage title div_obj chart_json

getExpLevelData :: ExpId -> Plate -> MType -> IO TimedPlotLinesData
getExpLevelData exp plate t = do
    exp_data <- liftIO $ loadExpDataDB exp (read plate)
    let sms = smoothAll bFiltS exp_data
    return $ timedMesToOdData sms t Nothing

getExpLevelCSV :: ExpId -> Plate -> MType -> GHandler RoboSite RoboSite RepHtml
getExpLevelCSV exp plate t = do
    pd <- liftIO $ getExpLevelData exp plate t
    let bytes = genCsvFile . plotLinesDataToStrings . M.map (M.map (map fst)) $ pd
    sendResponse (typePlain, toContent bytes)

getExpLevelGraph :: ExpId -> Plate -> MType -> GHandler RoboSite RoboSite RepHtml
getExpLevelGraph exp plate t = do
    pd <- liftIO $ getExpLevelData exp plate t
    let div_obj = "container"
    let page_title = t ++ ", " ++ plate ++ " - " ++ exp
    let title = "Expression level of " ++ t
    let subtitle = "Experiment: " ++ exp ++ ", Plate: " ++ plate
    let chart_json = [chartTitle title, chartSubtitle subtitle, chartXaxis "Time" (Just "datetime") Nothing, chartYaxis t Nothing Nothing, lineChart div_obj, chartLegend] ++ linesChartSeries pd
    graphPage title div_obj chart_json

getReadGraphData :: ExpId -> Plate -> MType -> IO TimedPlotLinesData
getReadGraphData exp plate t = do
    exp_data <- liftIO $ loadExpDataDB exp (read plate)
    return $ timedMesData exp_data t

getReadGraphCSV :: ExpId -> Plate -> MType -> GHandler RoboSite RoboSite RepHtml
getReadGraphCSV exp plate t = do
    pd <- liftIO $ getReadGraphData exp plate t
    let bytes = genCsvFile . plotLinesDataToStrings . M.map (M.map (map fst)) $ pd
    sendResponse (typePlain, toContent bytes)

getReadGraph :: ExpId -> Plate -> MType -> GHandler RoboSite RoboSite RepHtml
getReadGraph exp plate t = do
    pd <- liftIO $ getReadGraphData exp plate t
    let div_obj = "container"
    let page_title = t ++ ", " ++ plate ++ " - " ++ exp
    let title = "Measurement data of " ++ t
    let subtitle = "Experiment: " ++ exp ++ ", Plate: " ++ plate
    let chart_json = [chartTitle title, chartSubtitle subtitle, chartXaxis "Time" (Just "datetime") Nothing, chartYaxis t Nothing Nothing, lineChart div_obj, chartLegend] ++ linesChartSeries pd
    graphPage title div_obj chart_json

updatePlateLabel :: ExpId -> Int -> String -> IO ()
updatePlateLabel eid p l = do
    conn <- connectMySQL dbConnectInfo
    _ <- run conn "INSERT INTO tecan_plates (exp_id,plate,description) VALUES (?,?,?) ON DUPLICATE KEY UPDATE description = ?" [toSql eid, toSql p, toSql l, toSql l]
    return ()

updateExpLabel :: ExpId -> String -> IO ()
updateExpLabel eid l = do
    conn <- connectMySQL dbConnectInfo
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
                                                    \ #
                                                    <a href=@{ReadGraphCSV (fst exp) (fst plate) mtype}>
                                                        [Get data]
                                    <li>
                                        Expression levels
                                        <ul>
                                            $forall mtype <- expLevels (fst exp) (fst plate) disp_data
                                                <li>
                                                    <a href=@{ExpLevelGraph (fst exp) (fst plate) mtype}>
                                                        #{mtype}
                                                    \ #
                                                    <a href=@{ExpLevelCSV (fst exp) (fst plate) mtype}>
                                                        [Get data]
                                    <li>
                                        Grids
                                        <ul>
                                            $forall grid <- grids (fst exp) (fst plate) disp_data
                                                <li>
                                                    <a href=@{GridGraph (fst exp) (fst plate) (fst grid) (snd grid)}>
                                                        #{fst grid},#{snd grid}
                                                    \ #
                                                    <a href=@{GridGraphCSV (fst exp) (fst plate) (fst grid) (snd grid)}>
                                                        [Get data]
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

main = do
    warpDebug 3000 RoboSite
