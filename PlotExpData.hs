import Biolab.ExpData.Processing (rawMesData, intensityGridData)
import Biolab.Interfaces.Csv (processedDataToCSV, correlationDataToCSV, loadExpData)
import Biolab.Smoothing
import Biolab.Patches (has)
import Biolab.ExpData (expMesTypes)
import Biolab.Types (ExpData)
import GnuPlotUtils
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Maybe
import Data.List (intersect)
import Text.ParserCombinators.Parsec

data Options = Options {
    optOutput :: Maybe FilePath,
    optInput :: FilePath,
    optAxes :: (String,String)
} deriving Show

defaultOptions = Options {
    optOutput = Nothing,
    optInput = "",
    optAxes = ("MCHERRY","YFP")
}

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output"] (ReqArg (\f opts -> opts {optOutput = Just f}) "OUTPUT") "Optional output file name (default output is screen)",
    Option ['i'] ["input"] (ReqArg (\f opts -> opts {optInput = f}) "INPUT") "Input csv file",
    Option ['x'] ["axes"] (ReqArg (\x opts -> opts {optAxes = read x}) "AXES") "Grid axes - e.g. (\"YFP\",\"MCHERRY\")"
    ]

plotMesApp :: ExpData -> Maybe FilePath -> String -> IO ()
plotMesApp ed mfn t = do
    let pd = rawMesData t ed
    let	ofn = fmap (\x -> x ++ t ++ ".svg") mfn
    plotData t pd ofn 
    let	dfn = (fromMaybe ("graph") mfn) ++ t ++ "data.csv"
    let file_data = processedDataToCSV $ pd
    writeFile dfn file_data

plotGridApp :: ExpData -> (String,String) -> Maybe FilePath -> IO ()
plotGridApp ed axes mfn = do
    let ofn = fmap (\x -> x ++ "grid.svg") mfn
    plotIntensityGrid ed axes ofn
    let igd = intensityGridData axes ed
    let	dfn = (fromMaybe ("graph") mfn) ++ "Griddata.csv"
    let file_data = correlationDataToCSV $ igd
    writeFile dfn file_data

processOpt :: [String] -> Options
processOpt args = case getOpt RequireOrder options args of
	(o,_,[]) -> opt o
	(_,_,m) -> error $ usageInfo header options
	where
	    opt o = foldl (flip id) defaultOptions o
	    header = "Usage: PlotExpData [OPTION]"

main :: IO ()
main = do
    args <- getArgs
    let opt = processOpt args
    let input_file = optInput $ opt
    ms <- loadExpData input_file
    putStrLn $ "processing:" ++ input_file
    let mes_types = intersect (expMesTypes ms) ["MCHERRY","YFP","CFP"]
    mapM_ (plotMesApp ms (optOutput opt)) $ "OD600":mes_types
    let sms = smoothAll bFiltS ms
    if (mes_types `has` (fst . optAxes $ opt)) && (mes_types `has` (snd . optAxes $ opt))
	then plotGridApp sms (optAxes opt) (optOutput opt)
	else return ()
    return ()
