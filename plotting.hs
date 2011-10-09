import RoboAlg
import RoboDB
import Fitting
import RoboLib
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Packed.Vector as V
import Data.DateTime
import Data.Function
import Data.Maybe
import Graphics.Rendering.Plot
import Graphics.Rendering.Plot.Gtk
import Graphics.Rendering.Plot.Figure
import Graphics.Rendering.Plot.Figure.Simple

toPts :: String -> [Measurement] -> [(Double,Double)]
toPts mt = L.sortBy (compare `on` fst) . map (\m -> (fromIntegral . toSeconds . mTime $ m,mVal m)) . filter (\m -> mType m == mt)

normalizeTime :: (Fractional a) => [(a,b)] -> [(a,b)]
normalizeTime ((x1,y1):xys) = map (\(x,y) -> ((x-x1)/3600,y)) ((x1,y1):xys)

normVals :: (RealFloat a) => [(b,a)] -> [(b,a)]
normVals = filter (not . isNaN . snd)

xs = V.fromList . map fst
ys = V.fromList . map snd

displayLoop :: ExpData -> String -> PlotHandle -> IO ()
displayLoop ed wd h = do
    let w = Well {wRow = head wd, wColumn = read . tail $ wd}
    let mes = snd . fromJust . L.find (\(cid,_) -> cWell cid ==  w) $ concatMap M.toList . M.elems $ ed
    let ods = toPts "OD600" $ mes
    let yfs = toPts "YFP" $ mes
    let od_pts = normalizeTime ods
    let yf_pts = normalizeTime yfs
    let mc_pts = toPts "MCHERRY" mes
    let ods = V.fromList . map snd $ od_pts
    let log_ods = map (\(x,y) -> (x,logBase 2 y)) $ od_pts
    let log_yfp = map (\(x,y) -> (x,logBase 2 y)) $ yf_pts
    let od_fits = zip (map fst od_pts) $ expFitWindow 2.01 od_pts
    let yf_fits = zip (map fst yf_pts) $ expFitWindow 2.01 yf_pts
    let yf_alphas = normVals . map (\(x,y) -> (x,fdAlpha y)) $ yf_fits
    let yf_betas = normVals . map (\(x,y) -> (x,fdBeta y)) $ yf_fits
    let od_alphas = normVals . map (\(x,y) -> (x,fdAlpha y)) $ od_fits
    let od_betas = normVals . map (\(x,y) -> (x,fdBeta y)) $ od_fits
    let yf_exp = normVals $ expLevelsF od_pts yf_pts-- zipWith (\(x1,y1) (x2,y2) -> (x1,fdBeta y1 - fdBeta y2)) yf_fits od_fits
    withPlotHandle h $ do
        withTitle $ setText . wellStr $ w
        setPlots 2 2
        withPlot (1,1) $ do
            setDataset [
                (LinePoint, xs od_alphas, (ys od_alphas,Lower)),
                (LinePoint, xs yf_alphas,(ys yf_alphas,Lower)),
                (LinePoint, xs log_ods, (ys log_ods,Upper)),
                (LinePoint, xs log_yfp, (ys log_yfp,Upper))
                ]
            addAxis XAxis (Side Lower) $ do
                withAxisLabel . setText $ "time [hours]"
                setTicks Minor (Left 48)
                setGridlines Minor True
            addAxis YAxis (Side Lower) $ do
                withAxisLabel . setText $ "Growth rate"
            addAxis YAxis (Side Upper) $ do
                withAxisLabel . setText $ "Log measurement"
            setRangeFromData XAxis Lower Linear
            setRange YAxis Upper Linear (-10) 15
            setRange YAxis Lower Linear 0 1.5
            withHeading . setText $ "Log OD and Log YFP"
        withPlot (1,2) $ do
            setDataset [(LinePoint , xs od_betas,ys od_betas), (LinePoint, xs yf_betas, ys yf_betas)]
            addAxis XAxis (Side Lower) $ do
                withAxisLabel . setText $ "time [hours]"
                setTicks Minor (Left $ 47)
                setGridlines Minor True
            addAxis YAxis (Side Lower) $ do
                withAxisLabel . setText $ "Log measurement"
            setRangeFromData XAxis Lower Linear
            setRangeFromData YAxis Lower Linear
            withHeading . setText $ "Beta OD and Beta YFP"
        withPlot (2,1) $ do
            setDataset [(LinePoint , xs od_alphas, ys od_alphas),(LinePoint,xs yf_alphas, ys yf_alphas)]
            addAxis XAxis (Side Lower) $ do
                withAxisLabel . setText $ "time [hours]"
                setTicks Minor (Left $ 47)
                setGridlines Minor True
            addAxis YAxis (Side Lower) $ do
                withAxisLabel . setText $ "Growth rate"
            setRangeFromData XAxis Lower Linear
            setRangeFromData YAxis Lower Linear
            withHeading . setText $ "Growth rate and Fluorescense rate"
        withPlot (2,2) $ do
            setDataset [(LinePoint , xs yf_exp,ys yf_exp)]
            addAxis XAxis (Side Lower) $ do
                withAxisLabel . setText $ "time [hours]"
                setTicks Minor (Left $ 47)
                setGridlines Minor True
            addAxis YAxis (Side Lower) $ do
                withAxisLabel . setText $ "Expression level"
            setRangeFromData XAxis Lower Linear
            setRangeFromData YAxis Lower Linear
            withHeading . setText $ "exp level"
            -- setRange YAxis Lower Linear 10 17
    next <- getLine
    if next == "q"
        then return ()
        else displayLoop ed next h

main = do
    --ed <- loadExpDataDB "2011-09-27 17:07:57" 0
    --ed <- loadExpDataDB "2011-10-05 17:55:38" 2
    ed <- loadExpDataDB "2011-09-08 17:37:00" 6
    let edn = normalizePlate ed
    if (length . M.elems $ edn) == 0
        then return ()
        else do
            h <- display $ withTitle $ setText "my title"
            displayLoop edn "b2" h
