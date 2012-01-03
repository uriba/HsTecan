{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, TypeSynonymInstances, OverlappingInstances #-}
import Yesod
import Text.Hamlet
import Text.Julius
import Text.Cassius
import Yesod.Form.Fields (fileAFormReq)
import Yesod.Form.Jquery
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Database.HDBC.MySQL
import Database.HDBC
import Data.ByteString.UTF8 (toString)
import Data.List (nub, find, sort)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Math.Combinatorics.Graph (combinationsOf)
import Biolab.Interfaces.MySql (ExpDesc(..), PlateDesc(..), readTable, dbConnectInfo, loadExpDataDB)
import RoboLib (timedMesData, timedExpLevels, timedDoublingTimes, intensityGridData, estimatedData)
import Biolab.Interfaces.Csv (processedDataToCSV, correlationDataToCSV, measureDataToCSV)
import Biolab.Smoothing (bFiltS, smoothAll)
import Biolab.Types (Measurement(..), ExpId, MType, ldMap, CorrelationData, ProcessedData, LabeledData)
import Biolab.Patches (mapSnd, mapFst, mean)
import Biolab.Processing (minDoublingTimeMinutes, yield)
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
import Data.Either.Utils (forceEither, fromRight)
import Data.Text (Text)
import Data.Monoid
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
    return $ igd

getDoublingTimeCSV :: ExpId -> Plate -> MType -> Handler RepHtml
getDoublingTimeCSV exp plate t = do
    exp_data <- liftIO $ loadExpDataDB "RoboSite.conf" exp (read plate)
    let bytes = measureDataToCSV . estimatedData minDoublingTimeMinutes exp_data $ t
    sendResponse (typePlain, toContent bytes)

getYieldsCSV :: ExpId -> Plate -> MType -> Handler RepHtml
getYieldsCSV exp plate t = do
    exp_data <- liftIO $ loadExpDataDB "RoboSite.conf" exp (read plate)
    let bytes = measureDataToCSV . estimatedData yield exp_data $ t
    sendResponse (typePlain, toContent bytes)

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

getDoublingTimes :: ExpId -> Plate -> MType -> IO ProcessedData
getDoublingTimes exp plate t = do
    exp_data <- liftIO $ loadExpDataDB "RoboSite.conf" exp (read plate)
    return $ timedDoublingTimes t exp_data

getDoublingTimesGraph :: ExpId -> Plate -> MType -> Handler RepHtml
getDoublingTimesGraph exp plate t = do
    pd <- liftIO $ getDoublingTimes exp plate t
    let mpd = convertTimeToMS pd
    let div_obj = "container"
    let page_title = t ++ ", " ++ plate ++ " - " ++ exp
    let title = t ++ " Doubling times"
    let subtitle = "Experiment: " ++ exp ++ ", Plate: " ++ plate
    let chart_json = [chartTitle title, chartSubtitle subtitle, chartXaxis "Time" (Just "datetime") Nothing, chartYaxis t Nothing Nothing, lineChart div_obj, chartLegend, chartOptions] ++ linesChartSeries mpd
    graphPage title div_obj chart_json

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
    let div_obj = "container"
    let page_title = t ++ ", " ++ plate ++ " - " ++ exp
    let title = t ++ " Expression level"
    let subtitle = "Experiment: " ++ exp ++ ", Plate: " ++ plate
    let chart_json = [chartTitle title, chartSubtitle subtitle, chartXaxis "OD" Nothing Nothing, chartYaxis t Nothing Nothing, lineChart div_obj, chartLegend, chartOptions] ++ linesChartSeries pd
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

convertTimeToMS :: ProcessedData -> ProcessedData
convertTimeToMS = ldMap (G.map (mapFst (*1000)))
    
getTransformedReadGraph :: (Double -> Double) -> String -> ExpId -> Plate -> MType -> Handler RepHtml
getTransformedReadGraph f desc exp plate t = do
    pdt <- liftIO $ getReadGraphData exp plate t
    let pd = M.delete "NULL" pdt
    let mpd = convertTimeToMS . ldMap (G.map (mapSnd f)) $ pd
    let div_obj = "container"
    let page_title = t ++ ", " ++ plate ++ " - " ++ exp
    let title = t ++ " Measurement data" ++ " - " ++ desc
    let subtitle = "Experiment: " ++ exp ++ ", Plate: " ++ plate
    let chart_json = [chartTitle title, chartSubtitle subtitle, chartXaxis "Time" (Just "datetime") Nothing, chartYaxis t Nothing Nothing, lineChart div_obj, chartLegend, chartOptions] ++ linesChartSeries mpd
    graphPage title div_obj chart_json

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
expLevels e p = readings e p

grids :: ExpId -> Plate -> [GraphDesc] -> [(MType,MType)]
grids e p gd = zip (map head all_pairs) (map last all_pairs)
    where
        all_pairs = map (\x -> if head x == "OD600" then [last x, head x] else x) . combinationsOf 2 . expLevels e p $ gd

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
