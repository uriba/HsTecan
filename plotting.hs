import RoboLib
import Biolab.Interfaces.MySql (readTable, edExp, loadExpDataDB, dbConnectInfo)
import Biolab.Types (Measurement (..), ExpData, Well(..), ColonyId(..))
import Biolab.ExpData (normalizePlate)
import Biolab.Patches (mapSnd)
import Biolab.Constants (maturationTime)
import Biolab.Processing (expressionLevels)
import Biolab.Utils.Vector (expFitWindow, FitData(..))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Packed.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.DateTime
import Data.Function
import Data.Maybe
import System (getArgs)
import Graphics.Rendering.Plot
import Graphics.Rendering.Plot.Gtk
-- import Graphics.Rendering.Plot.Figure
-- import Graphics.Rendering.Plot.Figure.Simple

toPts :: String -> [Measurement] -> [(Double,Double)]
toPts mt = L.sortBy (compare `on` fst) . map (\m -> (fromIntegral . toSeconds . mTime $ m,mVal m)) . filter (\m -> mType m == mt)

normalizeTime :: (Fractional a) => [(a,b)] -> [(a,b)]
normalizeTime ((x1,y1):xys) = map (\(x,y) -> ((x-x1)/3600,y)) ((x1,y1):xys)

toLog :: (Floating a) => [(b,a)] -> [(b,a)]
toLog = map (mapSnd (logBase 2)) 

normVals :: (RealFloat a) => [(b,a)] -> [(b,a)]
normVals = map (\(x,y) -> if isNaN y then (x,0) else (x,y))

xs = V.fromList . map fst
ys = V.fromList . map snd

displayLoop :: ExpData -> String -> PlotHandle -> IO ()
displayLoop ed wd h = do
    let w = Well {wRow = head wd, wColumn = read . tail $ wd}
    let mes = snd . fromJust . L.find (\(cid,_) -> cWell cid ==  w) $ concatMap M.toList . M.elems $ ed
    let ods = toPts "OD600" $ mes
    let yfs = toPts "YFP" $ mes
    let mcs = toPts "MCHERRY" $ mes
    let od_pts = normalizeTime ods
    let yf_pts = normalizeTime yfs
    let mc_pts = normalizeTime mcs
    let log_ods = toLog od_pts
    let log_yfp = toLog yf_pts
    let log_mch = toLog mc_pts
    let od_fits = expFitWindow 2.01 . U.fromList $ od_pts
    let yf_fits = expFitWindow 2.01 . U.fromList $ yf_pts
    let mc_fits = expFitWindow 2.01 . U.fromList $ mc_pts
    let yf_alphas = normVals . map (\(x,y) -> (x,fdAlpha y)) $ yf_fits
    let yf_betas = normVals . map (\(x,y) -> (x,fdBeta y)) $ yf_fits
    let od_alphas = normVals . map (\(x,y) -> (x,fdAlpha y)) $ od_fits
    let od_betas = normVals . map (\(x,y) -> (x,fdBeta y)) $ od_fits
    let mc_alphas = normVals . map (\(x,y) -> (x,fdAlpha y)) $ mc_fits
    let mc_betas = normVals . map (\(x,y) -> (x,fdBeta y)) $ mc_fits
    let yf_exp = normVals . U.toList $ ((expressionLevels maturationTime) `on` U.fromList) od_pts yf_pts
    let mc_exp = normVals . U.toList $ ((expressionLevels maturationTime) `on` U.fromList) od_pts mc_pts
    withPlotHandle h $ do
        withTitle $ setText . wellStr $ w
        setPlots 1 1
{-        withPlot (1,1) $ do
            setDataset [
                (LinePoint, xs od_alphas, (ys od_alphas,Lower)),
                (LinePoint, xs yf_alphas,(ys yf_alphas,Lower)),
                (LinePoint, xs mc_alphas,(ys mc_alphas,Lower)),
                (LinePoint, xs log_ods, (ys log_ods,Upper)),
                (LinePoint, xs log_yfp, (ys log_yfp,Upper)),
                (LinePoint, xs log_mch, (ys log_mch,Upper))
                ]
            addAxis XAxis (Side Lower) $ do
                withAxisLabel . setText $ "time [hours]"
                setTicks Minor (Left $ length log_ods)
                setGridlines Minor True
            addAxis YAxis (Side Lower) $ do
                withAxisLabel . setText $ "Growth rate"
            addAxis YAxis (Side Upper) $ do
                withAxisLabel . setText $ "Log measurement"
            setRangeFromData XAxis Lower Linear
            setRange YAxis Upper Linear (-10) 15
            setRange YAxis Lower Linear 0 1.5
            withHeading . setText $ "Log OD, YFP, mCherry"
            setLegend True NorthEast Inside
        withPlot (1,2) $ do
            setDataset [
                (LinePoint, xs od_betas, ys od_betas),
                (LinePoint, xs yf_betas, ys yf_betas),
                (LinePoint, xs mc_betas, ys mc_betas)
                ]
            addAxis XAxis (Side Lower) $ do
                withAxisLabel . setText $ "time [hours]"
                setTicks Minor (Left $ length od_betas)
                setGridlines Minor True
            addAxis YAxis (Side Lower) $ do
                withAxisLabel . setText $ "Log measurement"
            setRangeFromData XAxis Lower Linear
            setRangeFromData YAxis Lower Linear
            withHeading . setText $ "Beta OD, YFP, mCherry"-}
        withPlot (1,1) $ do
            setDataset [
                (LinePoint, xs od_alphas, ys od_alphas),
                (LinePoint, xs yf_alphas, ys yf_alphas),
                (LinePoint, xs mc_alphas, ys mc_alphas)
                ]
            addAxis XAxis (Side Lower) $ do
                withAxisLabel . setText $ "time [hours]"
                setTicks Minor (Left $ length od_alphas)
                setGridlines Minor True
            addAxis YAxis (Side Lower) $ do
                withAxisLabel . setText $ "Growth rate"
            setRangeFromData XAxis Lower Linear
            setRangeFromData YAxis Lower Linear
            withHeading . setText $ "Growth rate and Fluorescense rate"
        {-withPlot (2,2) $ do
            setDataset [
                (LinePoint , xs yf_exp,ys yf_exp),
                (LinePoint , xs mc_exp,ys mc_exp)
                ]
            addAxis XAxis (Side Lower) $ do
                withAxisLabel . setText $ "time [hours]"
                setTicks Minor (Left $ length yf_exp)
                setGridlines Minor True
            addAxis YAxis (Side Lower) $ do
                withAxisLabel . setText $ "Expression level"
            setRangeFromData XAxis Lower Linear
            setRangeFromData YAxis Lower Linear
            withHeading . setText $ "exp level"-}
    next <- getLine
    if next == "q"
        then return ()
        else displayLoop ed next h

main = do
    [exp_id_p, pl] <- getArgs
    db_conf <- dbConnectInfo "RoboSite.conf"
    exp_descs <- readTable db_conf "tecan_experiments" $ Nothing
    let exp_ids = map edExp exp_descs
    putStrLn $ "searching for: " ++ exp_id_p
    let exp_id = fromJust . L.find (L.isPrefixOf exp_id_p) $ exp_ids
    putStrLn $ "found: " ++ exp_id
    --ed <- loadExpDataDB "2011-09-27 17:07:57" 0
    --ed <- loadExpDataDB "2011-10-05 17:55:38" 2
    --ed <- loadExpDataDB "2011-09-08 17:37:00" 6
    ed <- loadExpDataDB "RoboSite.conf" exp_id . read $ pl
    let edn = normalizePlate ed
    if (length . M.elems $ edn) == 0
        then do
            putStrLn $ "empty list"
            return ()
        else do
            h <- display $ withTitle $ setText (exp_id ++ ", Plate: " ++ pl)
            displayLoop edn "a1" h
