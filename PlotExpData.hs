import RoboLib
import System (getArgs)
import System.Console.GetOpt
import Data.Maybe
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
    Option ['x'] ["axes"] (ReqArg (\x opts -> opts {optAxes = read x}) "AXES") "Grid axes - e.g. (\"YFP\",\"MCHERRY\""
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

main :: IO ()
main = do
    args <- getArgs
    let (o,_,_) = getOpt RequireOrder options args
    let opt = foldl (flip id) defaultOptions o
    let input_file = optInput $ opt
    ms <- loadExpData input_file
    putStrLn $ "processing:" ++ input_file
    mapM_ (plotMesApp ms (optOutput opt)) ["OD600","MCHERRY","YFP","CFP"]
    mapM_ (plotMesToODApp ms (optOutput opt)) ["MCHERRY","YFP","CFP"]
    plotGridApp ms (optAxes opt) (optOutput opt)
    return ()
