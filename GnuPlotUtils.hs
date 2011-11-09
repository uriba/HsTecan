module GnuPlotUtils (
    plotData,
    plotIntensityGrid,
)
where

import Graphics.Gnuplot.Simple
import Biolab.Types (ExpData(..), MType)
import RoboLib
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

makePlotData :: (String, [[Double]]) -> Int -> [(PlotStyle, [Double])]
makePlotData (desc, vals) c = [ (defaultStyle {lineSpec = CustomStyle [LineTitle desc, LineType c]},x) | x <- vals ]

makePlotGridData :: (String,[(Double,Double)]) -> Int -> (PlotStyle, [(Double,Double)])
makePlotGridData (label,points) c =
    (defaultStyle {plotType = Points, lineSpec = CustomStyle [LineTitle label, PointType c, PointSize 2]},points)

plotData :: String -> PlotLinesData -> Maybe FilePath -> IO ()
plotData title pld m_fn = do
    let by_label = [ (label, M.elems x) | (label,x) <- M.toList pld ]
    let plot_data = concat . zipWith makePlotData by_label $ [1..]
    let fileoptions = fromMaybe [] . fmap fileOpts $ m_fn
    plotListsStyle ([Title title] ++ fileoptions) plot_data

plotMesData :: ExpData -> MType -> Maybe FilePath -> IO()
plotMesData ed  mt m_fn = plotData mt (mesData ed mt) m_fn

plotGrid :: String -> PlotGridData -> (String,String) -> Maybe FilePath -> IO ()
plotGrid title pgd (xtype,ytype) m_fn = plotPathsStyle plot_attrs plot_lines
    where
	file_options = fromMaybe [] . fmap fileOpts $ m_fn
	plot_attrs = [XLabel xtype, YLabel ytype, XRange (0,7), YRange (0,7)] ++ file_options
	labeled_grid_points = [ (label,M.elems pm) | (label,pm) <- M.toList pgd ]
	plot_lines = zipWith makePlotGridData labeled_grid_points [1..]

plotIntensityGrid :: ExpData -> (String, String) -> Maybe FilePath -> IO ()
plotIntensityGrid ed axes m_fn = plotGrid "Log scale grid plot" pgd axes m_fn
    where
	pgd = intensityGridData ed axes

fileOpts :: String -> [Attribute]
fileOpts fn = [ Custom "terminal" ["png", "size 1000,1000"], Custom "output" ["\"" ++ fn ++ "\""]]

