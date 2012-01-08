module RoboLib (
    timedMesData,
    timedExpLevels,
    timedDoublingTimes,
    intensityGridData,
    estimatedData,
    )
where
import qualified Data.Vector.Generic as G
import Biolab.Constants (maturationTime)
import Biolab.Types (ExpData, MType, ProcessedData, ldMap, MeasureData, CorrelationData, Measurement (..) )
import Biolab.Measurement (mesByTime, toPoint)
import Biolab.ExpData (normalizePlate, removeDeadWells)
import Biolab.Utils.Vector (Series, removeIllegalPoints)
import Biolab.Processing (expressionLevelEstimate, expressionLevels, minDoublingTimeMinutes, doublingTimeMinutes, doublingTimeMinutesPerOD)

series :: MType -> [Measurement] -> Series
series m = G.fromList . map toPoint . mesByTime m

timedMesData :: ExpData -> MType -> ProcessedData
timedMesData ed mt = ldMap (series mt) (normalizePlate ed)

timedDoublingTimes :: MType -> ExpData -> ProcessedData
timedDoublingTimes m ed = ldMap (\x -> removeIllegalPoints . doublingTimeMinutes . series m $ x) . normalizePlate  $ ed

timedExpLevels :: MType -> ExpData -> ProcessedData
timedExpLevels "OD600" ed = ldMap (\x -> removeIllegalPoints . doublingTimeMinutesPerOD . od_mes $ x) . normalizePlate $ ed
    where
        od_mes = series "OD600"
timedExpLevels m ed = ldMap (\x -> removeIllegalPoints . expressionLevels maturationTime (od_mes x) $ (series m x)) . normalizePlate $ ed
    where
        od_mes = series "OD600"

estimatedData :: (Series -> Double) -> ExpData -> String -> MeasureData
estimatedData f ed t = ldMap (f . series t) . normalizePlate $ ed

intensityGridData :: ExpData -> (String,String) -> CorrelationData
intensityGridData ed ("OD600",y) = intensityGridData ed (y, "OD600")
intensityGridData ed (xtype,"OD600") = ldMap (\x -> (exp_level xtype $ x, minDoublingTimeMinutes . series "OD600" $ x)) ned
    where
        ned = removeDeadWells . normalizePlate $ ed
        exp_level m ms = expressionLevelEstimate maturationTime (series "OD600" ms) (series m ms)
intensityGridData ed (xtype,ytype) = ldMap (\x -> (exp_level xtype x,exp_level ytype $ x)) ned
    where
        ned = removeDeadWells . normalizePlate $ ed
        exp_level mt ms = expressionLevelEstimate maturationTime (series "OD600" ms) (series mt ms)
