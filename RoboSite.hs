{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, TypeSynonymInstances, OverlappingInstances #-}
import Yesod
import Text.Hamlet
import Text.Julius
import Text.Cassius
import Text.Blaze (preEscapedText)
import Yesod.Form.Fields (fileAFormReq)
import Yesod.Form.Jquery
import Yesod.Auth
import Yesod.Auth.GoogleEmail
import Database.HDBC.MySQL
import Database.HDBC
import Data.ByteString.UTF8 (toString)
import Data.List (nub, find, sort, sortBy, findIndex)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Math.Combinatorics.Graph (combinationsOf)
import Biolab.Interfaces.MySql (ExpDesc(..), PlateDesc(..), readTable, dbConnectInfo, loadExpDataDB, WellDesc(..), SelectCriteria (..), fromNullString)
import Biolab.ExpData.Processing (expressionLevelData, rawMesData, timedExpLevels, timedDoublingTimes, intensityGridData, estimatedData, doublingTimeCorrelationData)
import Biolab.Interfaces.Csv (processedDataToCSV, correlationDataToCSV, measureDataToCSV)
import Biolab.Types (ExpData, MeasureData, ExpId, MType, ldMap, CorrelationData, ProcessedData, Well(..))
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
import Data.Traversable (sequenceA)
import Data.Monoid (mappend)
import Data.ConfigFile (emptyCP, readfile, options)
import Control.Monad.Error (runErrorT)
import Control.Arrow ((&&&))
import Monad (join)
import Data.Function (on)
import Data.Tuple.Utils (fst3, snd3, thd3)
import Char (toLower)

data GraphDesc = GraphDesc {gdExp :: ExpId, gdPlate :: Plate, gdMesType :: MType, gdExpDesc :: Maybe String, gdPlateDesc :: Maybe String} deriving (Show, Eq)

dbToGraphDescs :: [ExpDesc] -> [PlateDesc] -> [SqlValue] -> GraphDesc
dbToGraphDescs exp_descs plate_descs [SqlByteString exp_id, SqlInt32 p, SqlByteString mt] =
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
    return . map (dbToGraphDescs exp_descs plate_descs) $ sql_vals

data RoboSite = RoboSite
type Plate = String
type GraphType = String

mkYesod "RoboSite" $(parseRoutesFile "RoboRoutes")

instance Yesod RoboSite where
    approot _ = "http://localhost/"
    authRoute _ = Just $ AuthR LoginR
    isAuthorized _ True = isMember
    isAuthorized (UpdatePlateForm _ _ _) _ = isMember
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

-- add type and consider uniting with updatePlateForm
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

getExpLevelAtConstODCSV :: ExpId -> Plate -> MType -> Handler RepHtml
getExpLevelAtConstODCSV = getMeasureDataCSV (expressionLevelData)

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
    putStrLn $ "writing to row: " ++ show r ++ " and column: " ++ show c ++ " data:" ++ l
    _ <- run conn "INSERT INTO tecan_labels (exp_id,plate,row,col,label) VALUES (?,?,?,?,?) ON DUPLICATE KEY UPDATE label = ?" [toSql eid, toSql p, toSql r, toSql c, toSql l, toSql l]
    return ()

data PlateUpdateData = PlateUpdateData { pudCreateNewOwner :: Bool, pudExistingOwner :: Maybe T.Text, pudNewOwner :: Maybe T.Text, pudCreateNewProject :: Bool, pudExistingProject :: Maybe T.Text, pudNewProject :: Maybe T.Text, pudDesc :: T.Text}

plateUpdateForm :: Html -> MForm RoboSite RoboSite (FormResult PlateUpdateData,Widget)
plateUpdateForm = renderTable $ PlateUpdateData
    <$> areq boolField "Create new owner" (Just False)
    <*> aopt (selectField' . fieldVals $ "owner") "Owner" Nothing
    <*> aopt textField "New owner" Nothing
    <*> areq boolField "Create new project" (Just False)
    <*> aopt (selectField' . fieldVals $ "project") "Project" Nothing
    <*> aopt textField "New project" Nothing
    <*> areq textField "Description" Nothing

data ExpSelection = ExpSelection { esOwner :: Maybe Text, esProject :: Maybe Text, esEntries :: Maybe Int} deriving (Eq)

fieldVals :: String -> GGHandler RoboSite RoboSite IO (OptionList Text)
fieldVals f = do
    db_fields <- liftIO $ do
        db_conf <- dbConnectInfo "RoboSite.conf"
        conn <- connectMySQL db_conf
        f_list <- quickQuery' conn ("SELECT " ++ f ++ " FROM tecan_plates") []
        return . catMaybes . map fromNullString . concat $ f_list
    optionsPairs . map (T.pack &&& T.pack) $ db_fields

expSelectionFrom :: Maybe ExpSelection -> Html -> MForm RoboSite RoboSite (FormResult ExpSelection,Widget)
expSelectionFrom es = renderTable $ ExpSelection
        <$> aopt (selectField' . fieldVals $ "owner") "Owner" (esOwner <$> es)
        <*> aopt (selectField' . fieldVals $ "project") "Project" (esProject <$> es)
        <*> aopt intField "Entries to show" (esEntries <$> es)

getUpdatePlateForm :: String -> ExpId -> Plate -> Handler RepHtml
getUpdatePlateForm exists eid p = do
    ((_, widget), enctype) <- generateFormPost $ plateUpdateForm
    defaultLayout $ do
        if not . read $ exists
            then [whamlet| <p>Plate data must be updated before you can browse the experiment.|]
            else return ()
        $(whamletFile "UpdatePlateForm.whamlet")

pudErrors :: PlateUpdateData -> Maybe String
pudErrors pud = if  valid_owner == Nothing then valid_project else valid_owner
    where
        valid_owner = check_valid (pudCreateNewOwner pud) (pudNewOwner pud) (pudExistingOwner pud) "owner"
        valid_project = check_valid (pudCreateNewProject pud) (pudNewProject pud) (pudExistingProject pud) "project"
        check_valid create_new new_val selected_val desc = 
            if create_new
                then if new_val == Nothing || selected_val /= Nothing
                        then Just $ "Missing new " ++ desc ++ " name or illegal input"
                        else Nothing
                else if selected_val == Nothing || new_val /= Nothing
                        then Just $ "Missing " ++ desc ++ " selection or illegal input"
                        else Nothing

postUpdatePlateData :: ExpId -> Plate -> Handler RepHtml
postUpdatePlateData eid p = do
    ((res, widget), enctype) <- runFormPost plateUpdateForm
    case res of
        FormSuccess pud -> do
            let pud_errors = pudErrors pud
            if  pud_errors == Nothing
                then do
                    liftIO $ updatePlateData eid p pud
                    return "Plate data updated"
                    redirect RedirectTemporary $ ExpPage eid p
                else defaultLayout [whamlet|<p>Bad input: #{fromJust pud_errors}|]
        _ -> defaultLayout [whamlet|<p>Invalid input|]

updatePlateData :: ExpId -> Plate -> PlateUpdateData -> IO ()
updatePlateData eid p pud = do
    let owner = fromJust $ if pudCreateNewOwner pud then pudNewOwner pud else pudExistingOwner pud
    let project = fromJust $ if pudCreateNewProject pud then pudNewProject pud else pudExistingProject pud
    let updated_data = [toSql $ owner, toSql $ project, toSql $ pudDesc pud]
    db_conf <- dbConnectInfo "RoboSite.conf"
    conn <- connectMySQL db_conf
    _ <- run conn "INSERT INTO tecan_plates (exp_id,plate,owner,project,description) VALUES (?,?,?,?,?) ON DUPLICATE KEY UPDATE owner = ?, project = ?, description = ?" $ [toSql eid, toSql p] ++ updated_data ++ updated_data
    return ()

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

sqlToStr :: SqlValue -> String
sqlToStr (SqlByteString s) = toString s

dummyGraphDesc :: ExpId -> Plate -> [String] -> [GraphDesc]
dummyGraphDesc exp plate = map (\x -> GraphDesc {
                                gdExp = exp,
                                gdPlate = plate,
                                gdMesType = x,
                                gdExpDesc = Just "",
                                gdPlateDesc = Just ""
                                })

getExpPageData :: ExpId -> Plate -> IO (Maybe (PageExpDesc, [GraphDesc], [WellDesc]))
getExpPageData exp plate = do
    db_conf <- dbConnectInfo "RoboSite.conf"
    conn <- connectMySQL db_conf
    readings <- quickQuery' conn "SELECT DISTINCT reading_label FROM tecan_readings WHERE exp_id=? AND plate=?" [toSql exp, toSql plate]
    exp_desc <- quickQuery' conn "SELECT description FROM tecan_experiments WHERE exp_id = ?" [toSql exp]
    plate_desc_list <- quickQuery' conn "SELECT owner,project,description FROM tecan_plates WHERE exp_id = ? AND plate = ?" [toSql exp, toSql plate]
    well_labels <- readTable db_conf "tecan_labels" (Just $ SelectCriteria "where exp_id = ? AND plate = ?" [toSql exp, toSql plate])
    if null plate_desc_list || null well_labels
        then return Nothing
        else do
            let plate_desc = head plate_desc_list
            if plate_desc !! 0 == SqlNull || plate_desc !! 1 == SqlNull
                then
                    return Nothing
                else
                    return . Just $ (
                        PageExpDesc {
                            pedExp = exp,
                            pedPlate = plate,
                            pedPlateDesc = concat [sqlToStr . head . head $ exp_desc, ", ", sqlToStr $ plate_desc !! 2],
                            pedOwner = Just . sqlToStr $ plate_desc !! 0,
                            pedProject = Just . sqlToStr $ plate_desc !! 1
                        },
                        dummyGraphDesc exp plate . map (sqlToStr . head) $ readings,
                        well_labels)

wellDesc :: [WellDesc] -> Int -> Char -> String
wellDesc wells c r = fromMaybe "" . fmap wdDesc . find ((Well (toLower r) c ==) . wdWell) $ wells

allWells :: [(Int,Char)]
allWells = [(c,r) |  r <- ['a'..'h'], c <- [1..12]]

locateWell :: Int -> Char -> Int
locateWell x y = fromMaybe 0 . findIndex ((x,toLower y) ==) $ allWells

plateUpdateFormWidget :: [WellDesc] -> Html -> MForm RoboSite RoboSite (FormResult [Text],Widget)
plateUpdateFormWidget wds extra = do
    let rows = [1..12] :: [Int]
    let columns = ['A'..'H']
    (wRes,wViews) <- fmap unzip . mapM (\(c,r) -> mreq textField "unused" (Just . T.pack $ wellDesc wds c r)) $ allWells
    let res = sequenceA wRes
    let input x y = do
        let fvid = fvId $ wViews !! locateWell x y
        toWidget [lucius|
##{fvid} {
            width: 9em;
}
|]
        fvInput $ wViews !! locateWell x y
    let widget = do
        [whamlet|
#{extra}
            <h4>
                Plate layout:
            <table border="1">
                <th>
                $forall x <- rows
                    <th>
                        #{x}
                $forall y <- columns
                    <tr>
                        <th>
                            #{y}
                        $forall x <- rows
                            <td>
                                ^{input x y}
            <input type=submit value="Update">
            |]
    return (res, widget)

toStrings :: [Text] -> [[String]]
toStrings [] = []
toStrings xs = map T.unpack row : toStrings rest
    where
        (row,rest) = splitAt 12 xs

postExpPage e p = do
    getExpPage e p

getExpPage :: ExpId -> Plate -> Handler RepHtml
getExpPage exp plate = do
    maid <- maybeAuthId
    mdata <- liftIO $ getExpPageData exp plate
    if Nothing == mdata
        then
            redirect RedirectTemporary $ UpdatePlateForm (show False) exp plate
        else
            do
                let plate_desc = pedPlateDesc . fst3 . fromJust $ mdata
                let plate_owner = fromJust . pedOwner . fst3 . fromJust $ mdata
                let plate_project = fromJust . pedProject . fst3 . fromJust $ mdata
                let disp_data = snd3 . fromJust $ mdata
                let wells_desc = thd3 . fromJust $ mdata
                ((res,widget),enctype) <- runFormPost $ plateUpdateFormWidget wells_desc
                _ <- case res of
                    FormSuccess vals -> do
                        liftIO $ updatePlateLabels exp (read plate) (toStrings vals)
                        liftIO $ putStrLn "updated plate"
                    _ -> liftIO $ putStrLn "no data - plate not updated"
                liftIO . putStrLn . show $ length wells_desc
                let rows = [1..12]
                let columns = ['A'..'H']
                defaultLayout $ do
                    setTitle . toHtml . T.pack $ exp ++ " - " ++ plate
                    $(whamletFile "Header.whamlet")
                    $(whamletFile "ExpPage.hamlet")
                    addCassius $(cassiusFile "RoboSiteMain.cassius")

mTake :: Maybe Int -> [a] -> [a]
mTake Nothing = id
mTake (Just x) = take x

data PageExpDesc = PageExpDesc { pedExp :: ExpId, pedPlate :: Plate, pedPlateDesc :: String, pedOwner :: Maybe String, pedProject :: Maybe String} deriving (Eq)

expDesc :: [ExpDesc] -> PlateDesc -> PageExpDesc
expDesc eds pd = PageExpDesc {
        pedExp = pdExp pd,
        pedPlate = show . pdPlate $ pd,
        -- deprecate this and do the change in the DB. Then eds can be removed from this function.
        pedPlateDesc = concat [fromMaybe "" . fmap edDesc . find ((pdExp pd ==) . edExp) $ eds ,", ",pdDesc pd],
        pedOwner = pdOwner pd,
        pedProject = pdProject pd
    }

expDescFromReading :: [ExpDesc] -> [SqlValue] -> PageExpDesc
expDescFromReading eds [SqlByteString eid,SqlInt32 p] = PageExpDesc {
        pedExp = toString eid,
        pedPlate = show p,
        -- deprecate this and do the change in the DB. Then eds can be removed from this function.
        pedPlateDesc = fromMaybe "" . fmap edDesc . find ((toString eid ==) . edExp) $ eds,
        pedOwner = Nothing,
        pedProject = Nothing
    }

expsTable :: [PageExpDesc] -> Widget
expsTable exp_descs = toWidget $(hamletFile "ExpsTable.hamlet")

mEqual :: (Eq a) => Maybe a -> Maybe a -> Bool
mEqual Nothing _ = True
mEqual _ Nothing = False
mEqual (Just x) (Just y) = x == y

getExpsMain :: Handler RepHtml
getExpsMain = do
    maid <- maybeAuthId
    ((result, first_search_form), enctype) <- runFormGet $ expSelectionFrom Nothing
    (search_form, m_owner, m_project, m_num) <- case result of
        FormSuccess sd -> do
            ((_,sf),_) <- generateFormGet $ expSelectionFrom (Just sd)
            return ( sf, esOwner sd, esProject sd, esEntries sd)
        _ -> return (first_search_form, Nothing, Nothing, Just 20)
    db_conf <- liftIO $ dbConnectInfo "RoboSite.conf"
    -- remove this once plates are automatically inserted to the DB.
    conn <- liftIO $ connectMySQL db_conf
    reading_vals <- liftIO $ quickQuery' conn "SELECT DISTINCT exp_id,plate FROM tecan_readings" []
    exps <- liftIO $ readTable db_conf "tecan_plates" Nothing
    -- remove this once descriptions are plate-only.
    exp_table <- liftIO $ readTable db_conf "tecan_experiments" Nothing
    let exp_desc_from_plates = map (expDesc exp_table) exps
    -- all this ugly hack should be removed once the DB is simplified to include just one table that lists all plates, their experiments, descriptions etc.
    let exp_desc_from_readings = map (expDescFromReading exp_table) reading_vals
    let complete_exp_descs = exp_desc_from_plates ++ [ x | x <- exp_desc_from_readings, Nothing == find (\y -> pedExp y == pedExp x && pedPlate y == pedPlate x) exp_desc_from_plates]
    let relevant_exp_descs = filter (mEqual m_owner . fmap T.pack . pedOwner) . filter (mEqual m_project . fmap T.pack . pedProject) $ complete_exp_descs
    let exp_descs = mTake m_num . reverse . sortBy (compare `on` pedExp) $ relevant_exp_descs
    atomFeed <- liftIO . fmap T.pack . readFile $ "AtomFeedCode.html"
    let atomFeedWidget = toWidgetBody [hamlet|#{preEscapedText atomFeed}|]
    defaultLayout $ do
        setTitle "Robosite"
        $(whamletFile "Header.whamlet")
        $(whamletFile "RoboSiteMain.hamlet")
        toWidget $(cassiusFile "RoboSiteMain.cassius")
    
getHomeR :: Handler RepHtml
getHomeR = getExpsMain

main = do
    warpDebug 3000 RoboSite
