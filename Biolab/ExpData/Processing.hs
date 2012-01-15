module Biolab.ExpData.Processing (
    rawMesData,
    normedMesData,
    timedExpLevels,
    timedDoublingTimes,
    intensityGridData,
    doublingTimeCorrelationData,
    estimatedData,
    expressionLevelData,
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

rawMesData :: MType -> ExpData -> ProcessedData
rawMesData m = ldMap (series m)

normedMesData :: MType -> ExpData -> ProcessedData
normedMesData m = rawMesData m . normalizePlate

timedDoublingTimes :: MType -> ExpData -> ProcessedData
timedDoublingTimes m = ldMap (removeIllegalPoints . doublingTimeMinutes) . normedMesData m

timedExpLevels :: MType -> ExpData -> ProcessedData
timedExpLevels "OD600" ed = ldMap (removeIllegalPoints . doublingTimeMinutesPerOD) . normedMesData "OD600" $ ed
timedExpLevels m ed = ldZip expressionLevels (normedMesData "OD600" ed) (normedMesData m ed)

estimatedData :: (Series -> Double) -> MType -> ExpData -> MeasureData
estimatedData f m = ldMap f . normedMesData m

expressionLevelData :: MType -> ExpData -> MeasureData
expressionLevelData m ed = ldZip expressionLevelEstimate (normedMesData "OD600" ned) (normedMesData m ned)
    where
        ned = removeDeadWells $ ed

intensityGridData :: (MType,MType) -> ExpData -> CorrelationData
intensityGridData ("OD600",_) _ = error "unable to calculate expression level for OD measurements"
intensityGridData (_,"OD600") _ = error "unable to calculate expression level for OD measurements"
intensityGridData (x,y) ed = ldZip (,) (expressionLevelData x ed) (expressionLevelData y ed)

doublingTimeCorrelationData :: MType -> ExpData -> CorrelationData
doublingTimeCorrelationData t ed = ldZip (,) (expressionLevelData t ed) (estimatedData minDoublingTimeMinutes "OD600" ed)
