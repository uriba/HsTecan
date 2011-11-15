module RoboLib (
    mesData,
    timedMesData,
    timedExpLevels,
    intensityGridData,
    PlotLinesData,
    TimedPlotLinesData,
    PlotGridData,
    noTrans,
    wellStr,
    AxesTrans,
    )
where
import Data.Function (on)
import Data.List
import Data.Map ((!), Map(..), fromList)
import Data.Maybe
import Data.DateTime (DateTime, toSeconds)
import qualified Data.Map as M
import qualified Data.Vector.Generic as G
import Biolab.Constants
import Biolab.Types
import Biolab.Measurement (mesByTime, valByTime, filterBy)
import Biolab.ExpData
import Biolab.Utils.Vector (Series, Point)
import Biolab.Patches (mean, isLegal)
import Biolab.Processing (expressionLevelEstimate, expressionLevels, minDoublingTimeMinutes, doublingTimeMinutes)

-- Some types to make life easier...
type PlotLinesData = LabeledData [Double] -- for each label - a list of colonies, for each colony - a line.
type TimedPlotLinesData = LabeledData Series -- for each label - a list of colonies, for each colony - a line.
type PlotGridData = LabeledData (Double,Double) -- for each label - a list of colonies, for each colony - a line.

{-
autoFluorescenceMap :: ExpData -> Maybe MesTypeCorrectionVals
autoFluorescenceMap ed = do
    af_colonies <- M.lookup wildTypeId ed
    let af_mes = M.elems af_colonies
    let af m = mean . map (expLevel' m) $ af_mes
    return . M.fromList $ [(m, af m) | m <- expMesTypes ed]
    -}

timedMesData :: ExpData -> MType -> TimedPlotLinesData
timedMesData ed mt = ldMap (G.fromList . map (\x -> (fromIntegral . toSeconds . mTime $ x, mVal x)) . mesByTime mt) (normalizePlate ed)

mesData :: ExpData -> MType -> PlotLinesData
mesData ed mt = ldMap (G.toList . G.map snd) . timedMesData ed $ mt

removeDeadWells :: ExpData -> ExpData
removeDeadWells = M.filter (not . M.null) . M.map (M.filter liveWell)

removeIllegalPoints = G.filter (isLegal . snd)

toPoint :: Measurement -> Point
toPoint x = (fromIntegral . toSeconds . mTime $ x, mVal x)

timedExpLevels :: MType -> ExpData -> TimedPlotLinesData
timedExpLevels "OD600" ed = ldMap (\x -> removeIllegalPoints . doublingTimeMinutes . od_mes $ x) . normalizePlate  $ ed
    where
        od_mes = G.fromList . map toPoint . mesByTime "OD600"
timedExpLevels m ed = ldMap (\x -> removeIllegalPoints . expressionLevels maturationTime (od_mes x) $ (fl_mes m x)) . normalizePlate $ ed
    where
        fl_mes m = G.fromList . map toPoint . mesByTime m
        od_mes = fl_mes "OD600"

{-subtractAutoFluorescence :: ExpData -> ExpLevelData -- need adjustment for df/dt and maximum value selection
subtractAutoFluorescence ed = ldMap (map (\(mt,vals) -> (mt, map (corrected_el mt) vals))) . expLevels $ ed
    where
	corrected_el mt x = fromMaybe x $ do
	    afm <- autoFluorescenceMap ed
	    af <- M.lookup mt afm
	    return $ max (minValMap ! mt) (x - af)
-}
type AxesTrans = ((Double -> Double),(Double -> Double))

noTrans = (id,id)

intensityGridData :: ExpData -> (String,String) -> PlotGridData
intensityGridData ed ("OD600",y) = intensityGridData ed (y, "OD600")
intensityGridData ed (xtype,"OD600") = grid_points
    where
        ned = removeDeadWells . normalizePlate $ ed
        m_mes m = G.fromList . sortBy (compare `on` fst) . map (\x -> (fromIntegral . toSeconds . mTime $ x, mVal x)) . filter (\x -> mType x == m)
        exp_level m ms = expressionLevelEstimate maturationTime (m_mes "OD600" ms) (m_mes m ms)
        grid_points = ldMap (\x -> (exp_level xtype $ x, minDoublingTimeMinutes . m_mes "OD600" $ x)) ned

intensityGridData ed (xtype,ytype) = grid_points
    where
        ned = removeDeadWells . normalizePlate $ ed
        m_mes m = G.fromList . sortBy (compare `on` fst) . map (\x -> (fromIntegral . toSeconds . mTime $ x, mVal x)) . filter (\x -> mType x == m)
        exp_level mt ms = expressionLevelEstimate maturationTime (m_mes "OD600" ms) (m_mes mt ms)
        grid_points = ldMap (\x -> (exp_level xtype x,exp_level ytype $ x)) ned

wellStr :: Well -> String
wellStr w = concat ["(", [wRow w],",",show . wColumn $ w,")"]
