module Biolab.Constants (
    maxMes,
    wildTypeId,
    mediaId,
    minValMap,
    odLiveThreshold,
    odThreshold,
    stdMinOd,
)

where
import Data.Map
import Biolab.Types

maxMes :: Double
maxMes = 70000

wildTypeId = "WT" -- label of colonies that have wt bacteria (for auto-fluorescence cancellation)
mediaId = "BLANK"  -- label of colonies that have no bacteria (for background cancellation)

-- When subtracting background noise these are the minimal legal values.
minValMap :: MesTypeCorrectionVals
minValMap = fromList [("OD600",0.005), ("YFP", 20), ("MCHERRY",10),("CFP",10), ("GFP",10)]

odLiveThreshold = 0.2
odThreshold = 0.005
stdMinOd = 0.04

