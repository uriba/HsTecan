module Biolab.ExpData.Processing (
    timedMesData,
    timedExpLevels,
    timedDoublingTimes,
    intensityGridData,
    estimatedData,
    )
where
import qualified Data.Vector.Generic as G
import Biolab.Types (ExpData, MType, ProcessedData, ldMap, MeasureData, CorrelationData, Measurement, ldZip )
import Biolab.Measurement (mesByTime, toPoint)
import Biolab.ExpData (normalizePlate, removeDeadWells)
import Biolab.Utils.Vector (Series, removeIllegalPoints)
import Biolab.Processing (expressionLevelEstimate, expressionLevels, minDoublingTimeMinutes, doublingTimeMinutes, doublingTimeMinutesPerOD)
import Data.Map (intersectionWith)
import Data.Function (on)

series :: MType -> [Measurement] -> Series
series m = G.fromList . map toPoint . mesByTime m

timedMesData :: MType -> ExpData -> ProcessedData
timedMesData m = ldMap (series m) . normalizePlate

timedDoublingTimes :: MType -> ExpData -> ProcessedData
timedDoublingTimes m = ldMap (removeIllegalPoints . doublingTimeMinutes) . timedMesData m

timedExpLevels :: MType -> ExpData -> ProcessedData
timedExpLevels "OD600" ed = ldMap (\x -> removeIllegalPoints . doublingTimeMinutesPerOD . series "OD600" $ x) . normalizePlate $ ed
timedExpLevels m ed = ldMap (\x -> removeIllegalPoints . expressionLevels (series "OD600" $ x) $ (series m x)) . normalizePlate $ ed

estimatedData :: (Series -> Double) -> MType -> ExpData -> MeasureData
estimatedData f m = ldMap f . timedMesData m

expressionLevelData :: MType -> ExpData -> MeasureData
expressionLevelData m ed = ldZip expressionLevelEstimate (timedMesData "OD600" ed) (timedMesData m ed)

intensityGridData :: ExpData -> (String,String) -> CorrelationData
intensityGridData ed ("OD600",y) = intensityGridData ed (y,"OD600")
intensityGridData ed (x,"OD600") = ldZip (,) (expressionLevelData x ned) (estimatedData minDoublingTimeMinutes "OD600" ned)
    where
        ned = removeDeadWells . normalizePlate $ ed
intensityGridData ed (x,y) = ldZip (,) (expressionLevelData x ned) (expressionLevelData y ned)
    where
        ned = removeDeadWells . normalizePlate $ ed
