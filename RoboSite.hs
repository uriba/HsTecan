{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, TypeSynonymInstances, OverlappingInstances #-}
import Yesod
import Text.Hamlet
import Text.Julius
import Text.Cassius
import Yesod.Form.Fields (fileAFormReq)
import Yesod.Form.Jquery
import Yesod.Auth
import Yesod.Auth.GoogleEmail
import Database.HDBC.MySQL
import Database.HDBC
import Data.ByteString.UTF8 (toString)
import Data.List (nub, find, sort)
import Data.Maybe (fromMaybe)
import Math.Combinatorics.Graph (combinationsOf)
import Biolab.Interfaces.MySql (ExpDesc(..), PlateDesc(..), readTable, dbConnectInfo, loadExpDataDB)
import Biolab.ExpData.Processing (rawMesData, timedExpLevels, timedDoublingTimes, intensityGridData, estimatedData, doublingTimeCorrelationData)
import Biolab.Interfaces.Csv (processedDataToCSV, correlationDataToCSV, measureDataToCSV)
import Biolab.Types (ExpData, MeasureData, ExpId, MType, ldMap, CorrelationData, ProcessedData)
import Biolab.Patches (mapFst)
import Biolab.Processing (minDoublingTimeMinutes, yield)
import RoboSiteCore.Graph (rawMesGraph, logRawMesGraph, doublingTimesGraph, expLevelGraph, Graph(..), AxisDesc (..), AxisType (..), PlotData(..), expLevelsGrid, doublingTimeCorrelation)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector.Generic as G
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as U
import qualified Text.JSON as J
import Control.Applicative ((<$>), pure, (<*>))
import HighChartsJson
import Text.ParserCombinators.Parsec (parse)
import Data.CSV (csvFile)
import Data.Either.Utils (forceEither, fromRight)
import Data.Text (Text)
import Data.Monoid (mappend)
import Data.ConfigFile (emptyCP, readfile, options)
import Control.Monad.Error (runErrorT)
import Monad (join)

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
    approot _ = "http://localhost/"
    authRoute _ = Just $ AuthR LoginR
    isAuthorized _ True = isMember
    isAuthorized (UpdateExpForm _) _ = isMember
    isAuthorized (UpdatePlateForm _ _) _ = isMember
    isAuthorized (UploadPlateDesc _ _) _ = isMember
    isAuthorized _ _ = return Authorized

instance RenderMessage RoboSite FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery RoboSite

instance YesodAuth RoboSite where
    type AuthId RoboSite = Text
    getAuthId = return . Just . credsIdent
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins =
        [ authGoogleEmail ]

getUsers :: FilePath -> IO [String]
getUsers cf = do
    rv <- runErrorT $
        do
            cp <- join $ liftIO $ readfile emptyCP cf
            users <- options cp "USERS"
            return users
    return $ forceEither rv

isMember = do
    mu <- maybeAuthId
    liftIO $ putStrLn $ "is memeber called with uid:" ++ show mu
    authorized <- liftIO . getUsers $ "RoboSite.conf"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just x -> if x `elem` (map T.pack authorized)
                    then Authorized
                    else Unauthorized $ "User " `mappend` x `mappend` " not allowed to upload updates"

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
    liftIO . putStrLn . show $ parse csvFile "" $ (concatMap U.toString . BS.toChunks . fileContent . snd . head $ files) ++ "\n"
    let parsed = takeWhile (/= [""]) . fromRight . parse csvFile "" $ (concatMap U.toString . BS.toChunks . fileContent . snd . head $ files) ++ "\n"
    liftIO $ updatePlateLabels eid (read p) parsed
    liftIO . putStrLn . show $ parsed
    getHomeR

getExpData :: ExpId -> Plate -> IO ExpData
getExpData eid plate = loadExpDataDB "RoboSite.conf" eid (read plate)

getMeasureDataCSV :: (MType -> ExpData -> MeasureData) -> ExpId -> Plate -> MType -> Handler RepHtml
getMeasureDataCSV measure exp plate t = do
    exp_data <- liftIO $ getExpData exp plate
    let bytes = measureDataToCSV . measure t $ exp_data
    sendResponse (typePlain, toContent bytes)

getDoublingTimeCSV :: ExpId -> Plate -> MType -> Handler RepHtml
getDoublingTimeCSV = getMeasureDataCSV (estimatedData minDoublingTimeMinutes)

getYieldsCSV :: ExpId -> Plate -> MType -> Handler RepHtml
getYieldsCSV = getMeasureDataCSV (estimatedData yield)

getGridGraphCSV :: ExpId -> Plate -> MType -> MType -> Handler RepHtml
getGridGraphCSV exp plate x y = do
    exp_data <- liftIO $ getExpData exp plate
    let bytes = correlationDataToCSV . intensityGridData (x,y) $ exp_data
    sendResponse (typePlain, toContent bytes)

getGridGraph :: ExpId -> Plate -> MType -> MType -> Handler RepHtml
getGridGraph exp plate x y = getGraphPage (expLevelsGrid (x,y)) exp plate

getDoublingTimeExpLevelCSV :: ExpId -> Plate -> MType -> Handler RepHtml
getDoublingTimeExpLevelCSV exp plate x = do
    exp_data <- liftIO $ getExpData exp plate
    let bytes = correlationDataToCSV . doublingTimeCorrelationData x $ exp_data
    sendResponse (typePlain, toContent bytes)

getDoublingTimeExpLevel :: ExpId -> Plate -> MType -> Handler RepHtml
getDoublingTimeExpLevel exp plate x = getGraphPage (doublingTimeCorrelation x) exp plate

getDoublingTimesGraph :: ExpId -> Plate -> MType -> Handler RepHtml
getDoublingTimesGraph exp plate t = getGraphPage (doublingTimesGraph t) exp plate

getProcessedCSV :: (MType -> ExpData -> ProcessedData) -> ExpId -> Plate -> MType -> Handler RepHtml
getProcessedCSV process exp plate t = do
    exp_data <- liftIO $ getExpData exp plate
    let processed = process t exp_data
    sendResponse (typePlain, toContent . processedDataToCSV $ processed)

getExpLevelCSV :: ExpId -> Plate -> MType -> Handler RepHtml
getExpLevelCSV = getProcessedCSV timedExpLevels

getExpLevelGraph :: ExpId -> Plate -> MType -> Handler RepHtml
getExpLevelGraph exp plate t = getGraphPage (expLevelGraph t) exp plate

getReadGraphCSV :: ExpId -> Plate -> MType -> Handler RepHtml
getReadGraphCSV = getProcessedCSV rawMesData

getLogReadGraph :: ExpId -> Plate -> MType -> Handler RepHtml
getLogReadGraph exp plate t = getGraphPage (logRawMesGraph t) exp plate

getReadGraph :: ExpId -> Plate -> MType -> Handler RepHtml
getReadGraph exp plate t = getGraphPage (rawMesGraph t) exp plate

convertTimeToMS :: ProcessedData -> ProcessedData
convertTimeToMS = ldMap (G.map (mapFst (*1000)))

xAxisIsTime :: Graph -> Bool
xAxisIsTime g = Time == (axisType . graphXAxis $ g)

plotDataToHighChartsData :: Graph -> PlotData -> ([JSObj],Maybe String, (String -> JSObj))
plotDataToHighChartsData _ (GridPlot pd) = (gridChartSeries . M.delete "NULL" $ pd,Nothing, gridChart) -- grid don't use time axis
plotDataToHighChartsData g (LinePlot pd) = (chart_series,x_axis_type, lineChart)
    where
        (x_axis_type,mpd) = if xAxisIsTime g then (Just "datetime",convertTimeToMS pd) else (Nothing,pd)
        chart_series = linesChartSeries . M.delete "NULL" $ mpd

getGraphPage :: Graph -> ExpId -> Plate -> Handler RepHtml
getGraphPage g exp plate = do
    ed <- liftIO $ getExpData exp plate
    let (chart_series,x_axis_type, plot_type) = plotDataToHighChartsData g . graphGenerator g $ ed
    let div = "container"
    let page_title = graphTitle g
    let title = graphTitle g
    let subtitle = "Experiment: " ++ exp ++ ", Plate: " ++ plate
    let chart_json = [
            chartTitle title,
            chartSubtitle subtitle,
            chartXaxis (axisTitle . graphXAxis $ g) x_axis_type Nothing,
            chartYaxis (axisTitle . graphYAxis $ g) Nothing Nothing,
            plot_type div,
            chartLegend,
            chartOptions] ++ chart_series
    let json_data = J.encode . J.makeObj $ chart_json
    defaultLayout $ do
        setTitle . toHtml . T.pack $ title
        addJulius $(juliusFile "HighChartsGraph.julius")
        addHamlet $(hamletFile "GraphPage.hamlet")

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

labelUpdateForm :: Html -> MForm RoboSite RoboSite (FormResult Params,Widget)
labelUpdateForm = renderDivs $ Params <$> areq textField "Label" Nothing

getUpdatePlateForm :: ExpId -> Plate -> Handler RepHtml
getUpdatePlateForm eid p = updateForm eid (Just p)

getUpdateExpForm :: ExpId -> Handler RepHtml
getUpdateExpForm eid = updateForm eid Nothing

postUpdateLabel :: ExpId -> Plate -> Handler RepHtml
postUpdateLabel eid p = do
    ((res, widget), enctype) <- runFormPost labelUpdateForm
    output <-
        case res of
            FormMissing -> return . T.pack $ "Missing data!"
            FormFailure _ -> return . T.pack $ "Please correct the errors below."
            FormSuccess (Params label) -> do
                if p == "-1"
                    then
                        do
                            liftIO . updateExpLabel eid . T.unpack $ label
                            return "Label updated"
                    else
                        do
                            liftIO . updatePlateLabel eid (read p) . T.unpack $ label
                            return "Label updated"
    redirect RedirectTemporary HomeR

updateForm :: ExpId -> Maybe Plate -> Handler RepHtml
updateForm eid mp = do
    ((_, widget), enctype) <- generateFormPost $ labelUpdateForm
    defaultLayout $(whamletFile "UpdateLabelForm.whamlet")

plates :: ExpId -> [GraphDesc] -> [(Plate,Maybe String)]
plates eid gds = nub [(gdPlate x, gdPlateDesc x) | x <- gds, gdExp x == eid]

readings :: ExpId -> Plate -> [GraphDesc] -> [MType]
readings eid p = nub . map gdMesType . filter (\x -> gdExp x == eid && gdPlate x == p)

expLevels :: ExpId -> Plate -> [GraphDesc] -> [MType]
expLevels eid p = filter (/= "OD600") . readings eid p

grids :: ExpId -> Plate -> [GraphDesc] -> [(MType,MType)]
grids e p gd = zip (map head all_pairs) (map last all_pairs)
    where
        all_pairs = combinationsOf 2 . expLevels e p $ gd

getHomeR :: Handler RepHtml
getHomeR = do
    disp_data <- liftIO loadExps
    maid <- maybeAuthId
    let exp_descs =  reverse . sort . nub $ [ (gdExp x, gdExpDesc x) | x <- disp_data]
    let exp_ids = nub . map gdExp $ disp_data
    defaultLayout $ do
        setTitle "Robosite"
        addHamlet $(hamletFile "RoboSiteMain.hamlet")
        addJulius $(juliusFile "RoboSiteMain.julius")
        addCassius $(cassiusFile "RoboSiteMain.cassius")

main = do
    warpDebug 3000 RoboSite
