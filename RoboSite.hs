{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, TypeSynonymInstances, OverlappingInstances #-}
import Yesod
import Text.Hamlet
import Text.Julius
import Yesod.Form.Fields (fileAFormReq)
import Yesod.Form.Jquery
import Database.HDBC.MySQL
import Database.HDBC
import Data.ByteString.UTF8 (toString)
import Data.List (nub, find, sort)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Math.Combinatorics.Graph (combinationsOf)
import Biolab.Interfaces.MySql (ExpDesc(..), PlateDesc(..), readTable, dbConnectInfo, loadExpDataDB)
import RoboLib (timedMesData, timedExpLevels, intensityGridData) -- , ranges)
import Biolab.Interfaces.Csv (processedDataToCSV, correlationDataToCSV)
import Biolab.Smoothing (bFiltS, smoothAll)
import Biolab.Types (Measurement(..), ExpId, MType, ldMap, CorrelationData, ProcessedData, LabeledData)
import Biolab.Patches (mapSnd, mean)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Vector.Generic as G
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as U
import qualified Text.JSON as J
import Control.Applicative ((<$>), pure, (<*>))
import HighChartsJson
import Text.ParserCombinators.Parsec (parse)
import Data.CSV (csvFile)
import Data.Either.Utils

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

loadExps :: IO [GraphDesc]
loadExps = do
    db_conf <- dbConnectInfo "RoboSite.conf"
    conn <- connectMySQL db_conf
    sql_vals <- quickQuery' conn "SELECT DISTINCT exp_id,plate,reading_label FROM tecan_readings" []
    exp_descs <- readTable db_conf "tecan_experiments" Nothing
    plate_descs <- readTable db_conf "tecan_plates" Nothing
    return . map (dbToGraphDesc exp_descs plate_descs) $ sql_vals

data RoboSite = RoboSite
type Plate = String
type GraphType = String

mkYesod "RoboSite" $(parseRoutesFile "RoboRoutes")

instance Yesod RoboSite where
    approot _ = ""

instance RenderMessage RoboSite FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery RoboSite

-- update me
uploadPlateDesc = renderTable $ pure (,) <*> fileAFormReq "CSV file to upload:"

getUploadPlateDesc :: String -> String -> Handler RepHtml
getUploadPlateDesc eid p = do
    ((_,widget),enctype) <- generateFormPost uploadPlateDesc
    defaultLayout $(whamletFile "UploadPlateCsv.whamlet")

updatePlateLabels :: ExpId -> Int -> [[String]] -> IO ()
updatePlateLabels eid p labels = sequence_ update_labels
    where
        update_labels = zipWith (update_plate_row eid p) [0..] labels 
        update_plate_row e p r ls = sequence_ $ zipWith (updateWellLabel e p r) [0..] ls

postUploadedPlateDesc :: String -> String -> Handler RepHtml
postUploadedPlateDesc eid p = do
    (_,files) <- runRequestBody
    liftIO . putStrLn $ "got post request for eid" ++ eid ++ " and plate:" ++ p
    let parsed = fromRight . parse csvFile "" $ concatMap U.toString . BS.toChunks . fileContent . snd . head $ files
    liftIO $ updatePlateLabels eid (read p) parsed
    liftIO . putStrLn . show $ parsed
    getHomeR

graphPage :: String -> String -> [(String,J.JSValue)] -> Handler RepHtml
graphPage title div chart_json = do
    let json_data = J.encode . J.makeObj $ chart_json
    defaultLayout $ do
        setTitle . toHtml $ title
        addJulius $(juliusFile "HighChartsGraph.julius")
        addHamlet $(hamletFile "GraphPage.hamlet")

getGridGraphData :: ExpId -> Plate -> MType -> MType -> IO CorrelationData
getGridGraphData exp plate x y = do
    exp_data <- liftIO $ loadExpDataDB "RoboSite.conf" exp (read plate)
    let sms = smoothAll bFiltS exp_data
    let igd = intensityGridData sms (x,y) 
{-    putStrLn "avg is:"
    let as = concatMap M.elems [igd ! "AA",igd ! "AC",igd ! "AD"]
    let cs = concatMap M.elems [igd ! "CA",igd ! "CC",igd ! "CD"]
    let ds = concatMap M.elems [igd ! "DA",igd ! "DC",igd ! "DD"]
    let newmap = [("As", as), ("Cs", cs), ("Ds", ds)]
    let avgs = map (mapSnd $ (\cds -> (mean . filter ((70<)) . filter ((100>)) . map fst $ cds, mean . map snd $ cds))) newmap
    mapM (putStrLn . show) $ avgs
    putStrLn "median is:"
    let medians = map (mapSnd (\cds -> drop (length cds `div` 2) . sort . map fst $ cds)) newmap
    mapM (putStrLn . show) $ medians -}
    return $ igd

getGridGraphCSV :: ExpId -> Plate -> MType -> MType -> Handler RepHtml
getGridGraphCSV exp plate x y = do
    igd <- liftIO $ getGridGraphData exp plate x y
    let bytes = correlationDataToCSV $ igd
    sendResponse (typePlain, toContent bytes)

getGridGraph :: ExpId -> Plate -> MType -> MType -> Handler RepHtml
getGridGraph exp plate x y = do
    tgd <- liftIO $ getGridGraphData exp plate x y
    let igd = M.delete "NULL" tgd
    let div_obj = "container"
    let title = "(" ++ x ++ ", " ++ y ++ ")" ++ " Grid"
    let subtitle = "Experiment: " ++ exp ++ ", Plate: " ++ plate
    let chart_json = [chartTitle title, chartSubtitle subtitle, chartXaxis x Nothing Nothing, chartYaxis y Nothing Nothing, gridChart div_obj, chartLegend, chartOptions] ++ gridChartSeries igd
    graphPage title div_obj chart_json

{-getRangesData :: ExpId -> Plate -> MType -> IO (LabeledData (Double,Double))
getRangesData exp plate t = do
    exp_data <- liftIO $ loadExpDataDB "RoboSite.conf" exp (read plate)
    let sms = smoothAll bFiltS exp_data
    return $ ranges t sms
-}
getExpLevelData :: ExpId -> Plate -> MType -> IO ProcessedData
getExpLevelData exp plate t = do
    exp_data <- liftIO $ loadExpDataDB "RoboSite.conf" exp (read plate)
    let sms = smoothAll bFiltS exp_data
    return $ timedExpLevels t sms

getExpLevelCSV :: ExpId -> Plate -> MType -> Handler RepHtml
getExpLevelCSV exp plate t = do
    pd <- liftIO $ getExpLevelData exp plate t
    let bytes = processedDataToCSV pd
    sendResponse (typePlain, toContent bytes)

getExpLevelGraph :: ExpId -> Plate -> MType -> Handler RepHtml
getExpLevelGraph exp plate t = do
    pd <- liftIO $ getExpLevelData exp plate t
    -- ranges <- liftIO $ getRangesData exp plate t
    let div_obj = "container"
    let page_title = t ++ ", " ++ plate ++ " - " ++ exp
    let title = t ++ " Expression level"
    let subtitle = "Experiment: " ++ exp ++ ", Plate: " ++ plate
    let chart_json = [chartTitle title, chartSubtitle subtitle, chartXaxis "Time" (Just "datetime") Nothing, chartYaxis t Nothing Nothing, lineChart div_obj, chartLegend, chartOptions] ++ linesChartSeries pd -- markedLinesChartSeries pd ranges
    graphPage title div_obj chart_json

getReadGraphData :: ExpId -> Plate -> MType -> IO ProcessedData
getReadGraphData exp plate t = do
    exp_data <- liftIO $ loadExpDataDB "RoboSite.conf" exp (read plate)
    return $ timedMesData exp_data t

getReadGraphCSV :: ExpId -> Plate -> MType -> Handler RepHtml
getReadGraphCSV exp plate t = do
    pd <- liftIO $ getReadGraphData exp plate t
    let bytes = processedDataToCSV pd
    sendResponse (typePlain, toContent bytes)

getLogReadGraph :: ExpId -> Plate -> MType -> Handler RepHtml
getLogReadGraph = getTransformedReadGraph (logBase 2) "Log scale"

getReadGraph :: ExpId -> Plate -> MType -> Handler RepHtml
getReadGraph = getTransformedReadGraph id ""

getTransformedReadGraph :: (Double -> Double) -> String -> ExpId -> Plate -> MType -> Handler RepHtml
getTransformedReadGraph f desc exp plate t = do
    pdt <- liftIO $ getReadGraphData exp plate t
    let pd = M.delete "NULL" pdt
    let mpd = ldMap (G.map (mapSnd f)) pd
    let div_obj = "container"
    let page_title = t ++ ", " ++ plate ++ " - " ++ exp
    let title = t ++ " Measurement data" ++ " - " ++ desc
    let subtitle = "Experiment: " ++ exp ++ ", Plate: " ++ plate
    let chart_json = [chartTitle title, chartSubtitle subtitle, chartXaxis "Time" (Just "datetime") Nothing, chartYaxis t Nothing Nothing, lineChart div_obj, chartLegend, chartOptions] ++ linesChartSeries mpd
    graphPage title div_obj chart_json
    --let fitdata = M.map (M.map (growthRate t)) mpd
    --let fit = M.map (M.map (map (\(x,y) -> (1/((logBase 2 10) * x),y)) . expFit 7300)) fitdata
    --liftIO $ putStrLn . show $ fit

updateWellLabel :: ExpId -> Int -> Int -> Int -> String -> IO ()
updateWellLabel eid p r c l = do
    db_conf <- dbConnectInfo "RoboSite.conf"
    conn <- connectMySQL db_conf
    _ <- run conn "INSERT INTO tecan_labels (exp_id,plate,row,col,label) VALUES (?,?,?,?,?) ON DUPLICATE KEY UPDATE label = ?" [toSql eid, toSql p, toSql r, toSql c, toSql l, toSql l]
    return ()

updatePlateLabel :: ExpId -> Int -> String -> IO ()
updatePlateLabel eid p l = do
    db_conf <- dbConnectInfo "RoboSite.conf"
    conn <- connectMySQL db_conf
    _ <- run conn "INSERT INTO tecan_plates (exp_id,plate,description) VALUES (?,?,?) ON DUPLICATE KEY UPDATE description = ?" [toSql eid, toSql p, toSql l, toSql l]
    return ()

updateExpLabel :: ExpId -> String -> IO ()
updateExpLabel eid l = do
    db_conf <- dbConnectInfo "RoboSite.conf"
    conn <- connectMySQL db_conf
    _ <- run conn "INSERT INTO tecan_experiments (exp_id,description) VALUES (?,?) ON DUPLICATE KEY UPDATE description = ?" [toSql eid, toSql l, toSql l]
    return ()

data Params = Params {label :: T.Text}

labelUpdateForm :: Html -> Form RoboSite RoboSite (FormResult Params,Widget)
labelUpdateForm = renderDivs $ Params <$> areq textField "Label" Nothing

getUpdatePlateForm :: ExpId -> Plate -> Handler RepHtml
getUpdatePlateForm eid p = updateForm eid (Just p)

getUpdateExpForm :: ExpId -> Handler RepHtml
getUpdateExpForm eid = updateForm eid Nothing

getUpdateLabel :: ExpId -> Plate -> Handler RepHtml
getUpdateLabel eid p = do
    let m_label = Just . Params . T.pack $ "default"
    ((res, widget), enctype) <- runFormGet labelUpdateForm
    output <-
        case res of
            FormMissing -> return . T.pack $ "Missing data!"
            FormFailure _ -> return . T.pack $ "Please correct the errors below."
            FormSuccess (Params label) -> do
                if p == "-1"
                    then
                        liftIO . updateExpLabel eid . T.unpack $ label
                    else
                        liftIO . updatePlateLabel eid (read p) . T.unpack $ label
                return . T.pack $ "Label updated"
    getHomeR

updateForm :: ExpId -> Maybe Plate -> Handler RepHtml
updateForm eid mp = do
    ((_, widget), enctype) <- generateFormGet $ labelUpdateForm
    defaultLayout $(whamletFile "UpdateLabelForm.whamlet")

plates :: ExpId -> [GraphDesc] -> [(Plate,Maybe String)]
plates eid gds = nub [(gdPlate x, gdPlateDesc x) | x <- gds, gdExp x == eid]

readings :: ExpId -> Plate -> [GraphDesc] -> [MType]
readings eid p = nub . map gdMesType . filter (\x -> gdExp x == eid && gdPlate x == p)

expLevels :: ExpId -> Plate -> [GraphDesc] -> [MType]
expLevels e p = readings e p

grids :: ExpId -> Plate -> [GraphDesc] -> [(MType,MType)]
grids e p gd = zip (map head all_pairs) (map last all_pairs)
    where
        all_pairs = map (\x -> if head x == "OD600" then [last x, head x] else x) . combinationsOf 2 . expLevels e p $ gd

getHomeR :: Handler RepHtml
getHomeR = do
    disp_data <- liftIO loadExps
    let exp_descs =  reverse . sort . nub $ [ (gdExp x, gdExpDesc x) | x <- disp_data]
    let exp_ids = nub . map gdExp $ disp_data
    defaultLayout $ do
        setTitle "Robosite"
        addHamlet $(hamletFile "RoboSiteMain.hamlet")
        addJulius $(juliusFile "RoboSiteMain.julius")
        --addCassius $(cassiusFile "RoboSiteMain.cassius")

main = do
    warpDebug 3000 RoboSite
