module RoboSiteCore.Graph (
    transformXaxis,
    transformYaxis,
    Graph (..),
    AxisDesc(..),
    AxisType(..),
    rawMesGraph,
    logRawMesGraph,
    doublingTimesGraph,
    expLevelGraph,
    expLevelsGrid,
    PlotData(..),
)

where
import Biolab.ExpData.Processing (intensityGridData, rawMesData, timedDoublingTimes, timedExpLevels, normedMesData, )
import Biolab.ExpData (normalizePlate)
import Biolab.Types (ProcessedData, CorrelationData, ExpData, ldMap, MType)
import Biolab.Patches (mapFst, mapSnd)
import qualified Data.Vector.Generic as G

data PlotData = GridPlot CorrelationData | LinePlot ProcessedData
data Graph = Graph {
        graphGenerator :: ExpData -> PlotData,
        graphXAxis :: AxisDesc,
        graphYAxis :: AxisDesc,
        graphTitle :: String
    }

data AxisDesc = AxisDesc {
        axisTitle :: String,
        axisType :: AxisType
    }

data AxisType = Time | Values deriving Eq

rawMesGraph :: MType -> Graph
rawMesGraph t = Graph {
   graphGenerator = LinePlot . rawMesData t,
   graphXAxis = AxisDesc "Time" Time,
   graphYAxis = AxisDesc (t ++ " - arbitrary units") Values,
   graphTitle = t ++ " - raw measurements"
}

logRawMesGraph :: MType -> Graph
logRawMesGraph t = Graph {
   graphGenerator = LinePlot . ldMap (G.map (mapSnd (logBase 2))) . normedMesData t,
   graphXAxis = AxisDesc "Time" Time,
   graphYAxis = AxisDesc ("Log " ++ t ++ " (arbitrary units)") Values,
   graphTitle = "Log " ++ t ++ " measurements"
}

doublingTimesGraph :: MType -> Graph
doublingTimesGraph t = Graph {
   graphGenerator = LinePlot . timedDoublingTimes t,
   graphXAxis = AxisDesc "Time" Time,
   graphYAxis = AxisDesc ("Doubling time (minutes)") Values,
   graphTitle = "Doubling time - calculated based on " ++ t ++ " measurements"
}

expLevelGraph :: MType -> Graph
expLevelGraph t = Graph {
   graphGenerator = LinePlot . timedExpLevels t,
   graphXAxis = AxisDesc "OD" Values,
   graphYAxis = AxisDesc (t ++ " Expression level (arbitrary units)") Values,
   graphTitle = t ++ " Expression level"
}

expLevelsGrid :: (MType,MType) -> Graph
expLevelsGrid (tx,ty) = Graph {
   graphGenerator = GridPlot . intensityGridData (tx,ty),
   graphXAxis = AxisDesc ("Log " ++ tx ++ " Expression level") Values,
   graphYAxis = AxisDesc ("Log " ++ ty ++ " Expression level") Values,
   graphTitle = "(" ++ tx ++ "," ++ ty ++ ") Expression levels"
}

transformAxis :: ((Double,Double) -> (Double,Double)) -> PlotData -> PlotData
transformAxis f (GridPlot cd) = GridPlot . ldMap f $ cd
transformAxis f (LinePlot pd) = LinePlot . ldMap (G.map f) $ pd

transformXaxis :: (Double -> Double, String) -> Graph -> Graph
transformXaxis (f, prefix) g = g {
        graphGenerator = transformAxis (mapFst f) . graphGenerator g,
        graphXAxis = (graphXAxis g) {axisTitle = prefix ++ (axisTitle . graphXAxis $ g)}
    }

transformYaxis :: (Double -> Double, String) -> Graph -> Graph
transformYaxis (f, prefix) g = g {
        graphGenerator = transformAxis (mapSnd f) . graphGenerator g,
        graphXAxis = (graphYAxis g) {axisTitle = prefix ++ (axisTitle . graphYAxis $ g)}
    }
