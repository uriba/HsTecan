module HighChartsJson (
    chartTitle,
    chartSubtitle,
    chartXaxis,
    chartYaxis,
    gridChart,
    lineChart,
    chartLegend,
    chartOptions,
    gridChartSeries,
    linesChartSeries,
    colorsArray,
    JSObj,
)
where
import qualified Text.JSON as J
import qualified Data.Map as M
import qualified Data.Vector.Generic as G
import Text.Printf (printf)
import Biolab.Types (Well(..), Label, ColonyId(..), wellStr)
import Biolab.Utils.Vector
import Data.Maybe (fromMaybe, fromJust)
import Data.DateTime (toSeconds, DateTime)

type JSObj = (String, J.JSValue)

jsonString = J.showJSON . J.toJSString

chartTitle :: String -> JSObj
chartTitle t =  ("title", J.makeObj [
                    ("text", J.showJSON t),
                    ("x", J.showJSON (-20 :: Int))
                ])

chartSubtitle :: String -> JSObj
chartSubtitle st =  ("subtitle", J.makeObj [
                        ("text", J.showJSON st),
                        ("x", J.showJSON (-20 :: Int))
                    ])

chartAxis :: String -> String -> Maybe String -> Maybe (Double,Double) -> JSObj
chartAxis axis t atype m_limits =  (axis, J.makeObj $ [
                        ("title", J.makeObj [
                            ("text", jsonString t)
                        ]),
                        ("plotLines", J.showJSONs $ [
                            J.toJSObject [
                                ("value", J.showJSON (0 :: Int)),
                                ("width", J.showJSON (1 :: Int)),
                                ("color", jsonString "#808080")
                            ]
                        ]),
                        ("startOnTick", J.showJSON True),
                        ("gridLineWidth", J.showJSON (1 :: Int)),
                        ("endOnTick", J.showJSON True)
                    ]   ++ (fromMaybe [] . fmap (\(low,high) -> 
                                [ ("max", J.showJSON high), ("min", J.showJSON low)])
                            $ m_limits)
                        ++ (fromMaybe [] . fmap (\x -> [("type", jsonString x)]) $ atype)
                 )

chartXaxis = chartAxis "xAxis"
chartYaxis = chartAxis "yAxis"

chart :: String -> String -> JSObj
chart series_type html_object = ("chart", J.makeObj [
                                    ("renderTo", jsonString html_object),
                                    ("defaultSeriesType", jsonString series_type),
                                    ("animation", J.showJSON False),
                                    ("zoomType", jsonString "xy")
                                ])
gridChart :: String -> JSObj
gridChart = chart "scatter"

lineChart :: String -> JSObj
lineChart = chart "line"

chartOptions :: JSObj
chartOptions =   ("plotOptions", J.makeObj [
                    ("series", J.makeObj [
                        ("animation", J.showJSON False),
                        ("shadow", J.showJSON False)
                    ]),
                    ("line", J.makeObj [
                        ("marker", J.makeObj [
                            ("enabled", J.showJSON False)
                        ])
                    ])
                ])

chartLegend :: JSObj
chartLegend =   ("legend", J.makeObj [
                    ("layout", jsonString "horizontal"),
                    ("width", J.showJSON (300 :: Int)),
                    ("itemWidth", J.showJSON (150 :: Int)),
                    ("align", jsonString "right"),
                    ("verticalAlign", jsonString "top"),
                    ("x", J.showJSON (10 :: Int)),
                    ("y", J.showJSON (10 :: Int)),
                    ("borderWidth", J.showJSON (0 :: Int))
                ])

timeWiseGraphTitles :: String -> String -> String -> [JSObj]
timeWiseGraphTitles title subtitle yaxix = [
    ("xAxis", J.makeObj [
        ("type", jsonString "datetime"),
        ("title", J.makeObj [
            ("text", jsonString "Time")
        ])
    ])
    ]

indexedLables :: (Ord k) => M.Map k v -> M.Map (Int,k) v
indexedLables m = M.mapKeys (index_key indexed_keys) m
	where
		indexed_keys = zip (M.keys m) [0..]
		index_key ks k = (fromJust $ lookup k ks,k)

gridChartSeries :: M.Map Label (M.Map ColonyId (Double,Double)) -> [JSObj]
gridChartSeries pd = [("series", J.JSArray . concatMap (labelGridSeries (M.size pd)) $ M.toList . indexedLables $ pd)]

labelGridSeries :: Int -> ((Int,Label), (M.Map ColonyId (Double,Double))) -> [J.JSValue]
labelGridSeries s (l,vm) = map (gridPoint s l) . M.toList $ vm

gridPoint :: Int -> (Int,Label) -> (ColonyId, (Double, Double)) -> J.JSValue
gridPoint s (i,l) (cid,(x,y)) = J.makeObj [
    ("name", jsonString name),
    ("color", colorsArray s !! i),
    ("data", J.showJSONs [[x,y]])]
    where
        name = l ++ " - " ++ (wellStr . cWell $ cid)

linesChartSeries :: M.Map Label (M.Map ColonyId Series) -> [JSObj]
linesChartSeries pd = [("series", J.JSArray . concatMap (labelLinesSeries (M.size pd)) $ M.toList . indexedLables $ pd)]
    
labelLinesSeries :: Int -> ((Int,Label), (M.Map ColonyId Series)) -> [J.JSValue]
labelLinesSeries s (l,vm) = map (lineSeries s l) . M.toList $ vm

lineSeries :: Int -> (Int,Label) -> (ColonyId, Series) -> J.JSValue
lineSeries s (i,l) (cid,vals) = J.makeObj [
    ("name", jsonString name),
    ("color", colorsArray s !! i),
    ("data", J.showJSONs jsvals)]
    where
        name = l ++ " - " ++ (wellStr . cWell $ cid)
        jsvals = map (\(x,y) -> J.showJSONs [x,y]) . G.toList $ vals

colorsArray :: (Integral a) => a -> [J.JSValue]
colorsArray x = map (rgbColor . (\c -> (fromIntegral c) / fromIntegral x)) [1..x]

rgbColor :: Double -> J.JSValue
rgbColor h = rgbToStr . hsvTo256RGB $ (h,0.5,0.95)

rgbToStr :: (Int,Int,Int) -> J.JSValue
rgbToStr (r,g,b) = jsonString $ printf "#%02x%02x%02x" r g b

dmod :: (Integral a) => Double -> a -> Double
dmod x y = fromIntegral (floor x `mod` y) + (x - (fromIntegral . floor $ x))

hsvTo256RGB :: (Double,Double,Double) -> (Int,Int,Int)
hsvTo256RGB (h,s,v)= (norm r,norm g,norm b)
	where
		norm x = round $ 256*(x+m)
		h'=6*h
		c=v*s
		x=c*(1-abs((h' `dmod` 2) - 1))
		m = v - c
		(r,g,b) | h' < 1 = (c,x,0)
			| h' < 2 = (x,c,0)
			| h' < 3 = (0,c,x)
			| h' < 4 = (0,x,c)
			| h' < 5 = (x,0,c)
			| otherwise = (c,0,x)

-- sometime in the future add:
    {- ("tooltip", J.makeObj [
        ("formatter", J.JSString ("function() {return '<b>'+ this.series.name +'</b><br/>'+ this.y; }") :: String)
    ]), -}
