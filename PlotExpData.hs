import RoboLib
import RoboCSV
import GnuPlotUtils
import System (getArgs)
import System.Console.GetOpt
import Data.Maybe
import Data.List (intersect)
import Text.ParserCombinators.Parsec
import Data.CSV

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
    let pd = mesData ed t
    let	ofn = fmap (\x -> x ++ t ++ ".svg") mfn
    plotData t pd ofn 
    let	dfn = (fromMaybe ("graph") mfn) ++ t ++ "data.csv"
    let file_data = genCsvFile . plotLinesDataToStrings $ pd
    writeFile dfn file_data

plotMesToODApp :: ExpData -> Maybe FilePath -> String -> IO ()
plotMesToODApp ed mfn t = do
    let pd = mesToOdData ed t Nothing
    let	ofn = fmap (\x -> x ++ t ++ "toOD.svg") mfn
    plotData t pd ofn
    let	dfn = (fromMaybe ("graph") mfn) ++ t ++ "toODdata.csv"
    let file_data = genCsvFile . plotLinesDataToStrings $ pd
    writeFile dfn file_data

plotGridApp :: ExpData -> (String,String) -> Maybe FilePath -> IO ()
plotGridApp ed axes mfn = do
    let ofn = fmap (\x -> x ++ "grid.svg") mfn
    plotIntensityGrid ed axes (logBase 10, logBase 10) ofn
    let igd = intensityGridData ed axes noTrans
    let	dfn = (fromMaybe ("graph") mfn) ++ "Griddata.csv"
    let file_data = genCsvFile . plotGridDataToStrings $ igd
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
    mapM_ (plotMesToODApp sms (optOutput opt)) mes_types
    if (mes_types `has` (fst . optAxes $ opt)) && (mes_types `has` (snd . optAxes $ opt))
	then plotGridApp sms (optAxes opt) (optOutput opt)
	else return ()
    return ()
