module HighChartsJson (
    chartTitle,
    chartSubtitle,
    chartXaxis,
    chartYaxis,
    gridChart,
    lineChart,
    chartLegend,
    gridChartSeries,
    linesChartSeries,
)
where
import qualified Text.JSON as J
import qualified Data.Map as M
import RoboLib (Well(..), Label, PlotGridData, ColonyId(..), wellStr)
import Data.Maybe (fromMaybe)
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
                                    ("zoomType", jsonString "xy")
                                ])
gridChart :: String -> JSObj
gridChart = chart "scatter"

lineChart :: String -> JSObj
lineChart = chart "line"

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

gridChartSeries :: M.Map Label (M.Map ColonyId (Double,Double)) -> [JSObj]
gridChartSeries pd = [("series", J.JSArray . concatMap labelGridSeries $ M.toList pd)]

labelGridSeries :: (Label, (M.Map ColonyId (Double,Double))) -> [J.JSValue]
labelGridSeries (l,vm) = map (gridPoint l) . M.toList $ vm

gridPoint :: Label -> (ColonyId, (Double, Double)) -> J.JSValue
gridPoint l (cid,(x,y)) = J.makeObj [
    ("name", jsonString name),
    ("data", J.showJSONs [[x,y]])]
    where
        name = l ++ " - " ++ (wellStr . cWell $ cid)

linesChartSeries :: M.Map Label (M.Map ColonyId [(Double,DateTime)]) -> [JSObj]
linesChartSeries pd = [("series", J.JSArray . concatMap labelLinesSeries $ M.toList pd)]
    
labelLinesSeries :: (Label, (M.Map ColonyId [(Double,DateTime)])) -> [J.JSValue]
labelLinesSeries (l,vm) = map (lineSeries l) . M.toList $ vm

lineSeries :: Label -> (ColonyId, [(Double, DateTime)]) -> J.JSValue
lineSeries l (cid,vals) = J.makeObj [
    ("name", jsonString name),
    ("data", J.showJSONs jsvals)]
    where
        name = l ++ " - " ++ (wellStr . cWell $ cid)
        jsvals = map (\(x,y) -> J.showJSONs [fromIntegral $ toSeconds y * 1000,x]) vals

-- sometime in the future add:
    {- ("tooltip", J.makeObj [
        ("formatter", J.JSString ("function() {return '<b>'+ this.series.name +'</b><br/>'+ this.y; }") :: String)
    ]), -}
