module Biolab.ExpData (
    normalizePlate,
    liveWell,
    expMesTypes,
)
where

import Biolab.Types
import Biolab.Constants
import Biolab.Patches
import Biolab.Measurement
import Data.Map ((!), Map(..), fromList)
import Data.List (nub)
import qualified Data.Map as M

constantBackgroundMap :: ExpData -> MesTypeCorrectionVals
constantBackgroundMap ed = fromList [(m, bg m) | m <- mes_types]
    where
	bg m = mean . map mVal . filterByType m $ bg_mes 
	mes_types = expMesTypes ed
	bg_mes = concat . filter (not . liveWell) . M.elems $ (ed ! mediaId)

subtractConstantBackground :: ExpData -> ExpData
subtractConstantBackground ed = M.map (M.map (map (\x -> x {mVal = corrected_mval x}))) ed
    where
	background_map = constantBackgroundMap ed
	corrected_mval m =
	    let mt = mType m in max (minValMap ! mt) (mVal m - (background_map ! mt))

expMesTypes :: ExpData -> [String]
expMesTypes = nub . map mType . concat . concatMap M.elems . M.elems

normalizePlate :: ExpData -> ExpData
normalizePlate ed
    | Nothing == M.lookup mediaId ed = M.map (M.map (map (\x -> if mType x == "OD600" then x {mVal = max (minValMap ! "OD600") (mVal x - stdMinOd)} else x))) $ ed
    | otherwise = subtractConstantBackground ed

liveWell :: [Measurement] -> Bool -- returns whether measurements taken from a given well indicate that it grew.
liveWell ms = (last . valByTime "OD600" $ ms) > odLiveThreshold

