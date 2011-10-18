module Biolab.Smoothing (
    smoothAll,
    bFiltS,
)
where
import Biolab.Types
import Data.List (sortBy, tails, partition)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import RoboAlg (mean)
import qualified Data.Map as M

windowSize = 5

type SmoothDataset = [Double] -> [Double]

changeVal :: Measurement -> Double -> Measurement
changeVal m x = m {mVal = x}

smoothMes :: SmoothDataset -> [Measurement] -> [Measurement]
smoothMes f ms = zipWith ($) (map changeVal ordered_data) . f . map mVal $ ordered_data
    where
	ordered_data = sortBy (compare `on` mTime) ms

applySmooth' :: SmoothDataset -> MType -> [Measurement] -> [Measurement]
applySmooth' f mt ms = not_affected ++ smoothed
    where
	(affected,not_affected) = partition ((==) mt . mType) ms
	smoothed = smoothMes f affected

applySmooth :: SmoothDataset -> MType -> ExpData -> ExpData
applySmooth f mt = M.map (M.map (applySmooth' f mt))

smoothAll :: SmoothDataset -> ExpData -> ExpData
smoothAll f ed = foldr (applySmooth f) ed ["OD600","MCHERRY","YFP","CFP"]

bFiltS :: SmoothDataset
bFiltS ds = mapMaybe (bFilt . take windowSize) . tails $ extended_dataset
    where
	extended_dataset = header ++ ds ++ trailer
	header = replicate buf_size . head $ ds
	trailer = replicate buf_size . last $ ds
	buf_size = windowSize `div` 2

bFilt :: [Double] -> Maybe Double
bFilt xs
    | length xs < windowSize = Nothing
    | null relevant_data = val xs
    | otherwise = val relevant_data
	where
	    mx = maximum xs
	    mn = minimum xs
	    val = Just . mean 
	    relevant_data = filter (\x -> x < mx && x > mn) $ xs

