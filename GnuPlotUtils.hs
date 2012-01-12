module GnuPlotUtils (
    plotData,
    plotIntensityGrid,
)
where

import Graphics.Gnuplot.Simple
import Biolab.Types (ExpData(..), MType, ProcessedData, CorrelationData)
import Biolab.Utils.Vector (Point)
import Biolab.ExpData.Processing (rawMesData, intensityGridData)
import qualified Data.Map as M
import qualified Data.Vector.Generic as G
import Data.Maybe (fromMaybe)

makePlotData :: (String, [[Point]]) -> Int -> [(PlotStyle, [Point])]
makePlotData (desc, vals) c = [ (defaultStyle {lineSpec = CustomStyle [LineTitle desc, LineType c]},x) | x <- vals ]

makePlotGridData :: (String,[(Double,Double)]) -> Int -> (PlotStyle, [(Double,Double)])
makePlotGridData (label,points) c =
    (defaultStyle {plotType = Points, lineSpec = CustomStyle [LineTitle label, PointType c, PointSize 2]},points)

plotData :: String -> ProcessedData -> Maybe FilePath -> IO ()
plotData title pld m_fn = do
    let by_label = [ (label, map G.toList . M.elems $ x) | (label,x) <- M.toList pld ]
    let plot_data = concat . zipWith makePlotData by_label $ [1..]
    let fileoptions = fromMaybe [] . fmap fileOpts $ m_fn
    plotPathsStyle ([Title title] ++ fileoptions) plot_data

plotTimedMesData :: ExpData -> MType -> Maybe FilePath -> IO()
plotTimedMesData ed  mt m_fn = plotData mt (rawMesData mt ed) m_fn

plotGrid :: String -> CorrelationData -> (String,String) -> Maybe FilePath -> IO ()
plotGrid title pgd (xtype,ytype) m_fn = plotPathsStyle plot_attrs plot_lines
    where
	file_options = fromMaybe [] . fmap fileOpts $ m_fn
	plot_attrs = [XLabel xtype, YLabel ytype, XRange (0,7), YRange (0,7)] ++ file_options
	labeled_grid_points = [ (label,M.elems pm) | (label,pm) <- M.toList pgd ]
	plot_lines = zipWith makePlotGridData labeled_grid_points [1..]

plotIntensityGrid :: ExpData -> (String, String) -> Maybe FilePath -> IO ()
plotIntensityGrid ed axes m_fn = plotGrid "Log scale grid plot" pgd axes m_fn
    where
	pgd = intensityGridData axes ed

fileOpts :: String -> [Attribute]
fileOpts fn = [ Custom "terminal" ["png", "size 1000,1000"], Custom "output" ["\"" ++ fn ++ "\""]]

