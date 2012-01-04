module RoboSiteCore.Graph (
    transformXaxis,
    transformYaxis,
    simpleGraph,
)

where
import RoboLib (timedMesData)
import Biolab.Types (ProcessedData, CorrelationData, ExpData, ldMap, MType)
import Biolab.Patches (mapFst, mapSnd)
import qualified Data.Vector.Generic as G

data PlotData = GridPlot CorrelationData | LinePlot ProcessedData
data Graph = Graph { graphGenerator :: ExpData -> PlotData, graphXAxis :: AxisDesc, graphYAxis :: AxisDesc, graphTitle :: String }
data AxisDesc = AxisDesc {axisTitle :: String, axisType :: AxisType}
data AxisType = Time | Values

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

simpleGraph :: MType -> Graph
simpleGraph t = Graph {
        graphGenerator = LinePlot . flip timedMesData t, -- update this when refactoring timedMesData
        graphXAxis = AxisDesc "Time" Time,
        graphYAxis = AxisDesc t Values,
        graphTitle = t
    }
