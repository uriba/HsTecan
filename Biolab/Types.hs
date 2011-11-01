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
)
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.DateTime (DateTime)

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
