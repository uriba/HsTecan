import RoboLib
import System (getArgs)
import System.Console.GetOpt

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
plotMesApp ed mfn t = plotMesData ed t ofn
    where
	ofn = fmap (\x -> x ++ t ++ ".svg") mfn

main :: IO ()
main = do
    args <- getArgs
    let (o,_,_) = getOpt RequireOrder options args
    let opt = foldl (flip id) defaultOptions o
    let input_file = optInput $ opt
    ms <- loadExpData input_file
    putStrLn $ "processing:" ++ input_file
    mapM_ (plotMesApp ms (optOutput opt)) ["OD600","MCHERRY","YFP","CFP"]
    return ()
