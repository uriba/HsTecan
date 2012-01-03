module Biolab.Types (
    MType,
    MesTypeCorrectionVals,
    Label,
    ExpId,
    PlateId,
    Well(..),
    ColonyId(..),
    Measurement(..),
    LabeledData(..),
    ldMap,
    ExpData,
    colonyId,
    Seconds,
    ProcessedData,
    CorrelationData,
    MeasureData,
    PlotData,
    ExportedData,
    wellStr,
)
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.DateTime (DateTime)
import Biolab.Utils.Vector (Series)

type Seconds = Integer
type MType = String
type Label = String
type ExpId = String
type PlateId = Int
type MesTypeCorrectionVals = Map MType Double

data Well = Well {
        wRow :: Char,
        wColumn :: Int
    } deriving (Eq, Show, Read, Ord)

wellStr :: Well -> String
wellStr w = concat ["(", [wRow w],",",show . wColumn $ w,")"]

data ColonyId = ColonyId {
        cExp :: ExpId,
        cPlate :: Int,
        cWell :: Well
    } deriving (Eq, Show, Ord)

data Measurement = Measurement {
        mExpDesc :: ExpId,
        mPlate :: PlateId,
        mTime :: DateTime,
        mType :: MType,
        mWell :: Well,
        mLabel :: Label,
        mVal :: Double
    } deriving (Eq, Show)

colonyId :: Measurement -> ColonyId
colonyId m = ColonyId {
        cExp = mExpDesc m,
        cPlate = mPlate m,
        cWell = mWell m
    }

type LabeledData a = Map Label (Map ColonyId a)
    
ldMap :: (a -> b) -> LabeledData a -> LabeledData b
ldMap f = M.map (M.map f)

type ExpData = LabeledData [Measurement] -- experiment data is mapped like this.
type ProcessedData = LabeledData Series -- for each label - a list of colonies, for each colony - a line.
type CorrelationData = LabeledData (Double,Double) -- for each label - a list of colonies, for each colony - a line.
type ExportedData = LabeledData [String]
type MeasureData = LabeledData Double
data PlotData = LinesData ProcessedData | GridData CorrelationData
