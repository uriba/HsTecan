module Biolab.Constants (
    maxMes,
    wildTypeId,
    mediaId,
    minValMap,
    odLiveThreshold,
    stdMinOd,
    exponentialPhaseGrowthRateWindow,
    maturationTime,
)

where
import Data.Map
import Biolab.Types

-- plate-reader constants
maxMes :: Double
maxMes = 70000

wildTypeId = "WT" -- label of colonies that have wt bacteria (for auto-fluorescence cancellation)
mediaId = "BLANK"  -- label of colonies that have no bacteria (for background cancellation)

-- When subtracting background noise these are the minimal legal values.
minValMap :: MesTypeCorrectionVals
minValMap = fromList [("OD600",0.005), ("YFP", 20), ("MCHERRY",10),("CFP",10), ("GFP",10)]

odLiveThreshold = 0.05
stdMinOd = 0.04

-- biological constants
exponentialPhaseGrowthRateWindow :: Double
exponentialPhaseGrowthRateWindow = 2 * 3600 + 60 -- to make sure the window includes the last measurement
maturationTime = 1800 -- estimated fluorophore maturation time
