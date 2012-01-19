module Biolab.ExpData (
    normalizePlate,
    liveWell,
    removeDeadWells,
    expMesTypes,
    createExpData,
    series,
)
where

import Biolab.Types
import Biolab.Constants
import Biolab.Patches
import Biolab.Measurement
import Biolab.ExpData.Utils (series, expMesTypes, removeDeadWells, liveWell)
import Biolab.ExpData.AutoFluorescense (subtractAutoFluorescense)
import Biolab.Utils.Vector (Series, Point)
import Biolab.Processing (expressionLevels)
import Data.Map ((!), Map(..), fromList)
import Data.List (nub, sortBy, minimumBy, intersect)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Vector.Generic as G
import Data.DateTime (toSeconds)

constantBackgroundMap :: ExpData -> MesTypeCorrectionVals
constantBackgroundMap ed = fromList [(m, bg m) | m <- mes_types]
    where
        bg m = mean . map mVal . filterByType m $ bg_mes 
        mes_types = expMesTypes ed
        bg_mes = concat . filter (not . liveWell) . concatMap M.elems . catMaybes . map (flip M.lookup ed) $ mediaId

subtractConstantBackground :: ExpData -> ExpData
subtractConstantBackground ed = ldMap (map (\x -> x {mVal = corrected_mval x})) ed
    where
        background_map = constantBackgroundMap ed
        corrected_mval m =
            let mt = mType m in max (minValMap ! mt) (mVal m - (background_map ! mt))

normalizePlate :: ExpData -> ExpData
normalizePlate ed
    | [] == M.keys ed `intersect` mediaId = ed
    | otherwise = subtractAutoFluorescense . subtractConstantBackground $ ed

createExpData :: [Measurement] -> ExpData
createExpData ms = fromList [ (label, m_for_label label) | label <- labels ms]
    where
	labels = nub . map mLabel
	colonies l = nub . map colonyId . filterBy mLabel l
	m_for_colony cid = filterBy colonyId cid ms
	m_for_label l = fromList [ (colony_id, m_for_colony colony_id) | colony_id <- colonies l ms ]
