module RoboLib (
    timedMesData,
    timedExpLevels,
    intensityGridData,
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
import Biolab.Measurement (mesByTime, valByTime, filterBy, toPoint)
import Biolab.ExpData
import Biolab.Utils.Vector (Series, Point)
import Biolab.Patches (mean, isLegal)
import Biolab.Processing (expressionLevelEstimate, expressionLevels, minDoublingTimeMinutes, doublingTimeMinutes)

-- Some types to make life easier...

timedMesData :: ExpData -> MType -> ProcessedData
timedMesData ed mt = ldMap (G.fromList . map (\x -> (fromIntegral . toSeconds . mTime $ x, mVal x)) . mesByTime mt) (normalizePlate ed)

removeDeadWells :: ExpData -> ExpData
removeDeadWells = M.filter (not . M.null) . M.map (M.filter liveWell)

removeIllegalPoints = G.filter (isLegal . snd)

timedExpLevels :: MType -> ExpData -> ProcessedData
timedExpLevels "OD600" ed = ldMap (\x -> removeIllegalPoints . doublingTimeMinutes . od_mes $ x) . normalizePlate  $ ed
    where
        od_mes = G.fromList . map toPoint . mesByTime "OD600"
timedExpLevels m ed = ldMap (\x -> removeIllegalPoints . expressionLevels maturationTime (od_mes x) $ (fl_mes m x)) . normalizePlate $ ed
    where
        fl_mes m = G.fromList . map toPoint . mesByTime m
        od_mes = fl_mes "OD600"

type AxesTrans = ((Double -> Double),(Double -> Double))

intensityGridData :: ExpData -> (String,String) -> CorrelationData
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
