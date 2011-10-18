module Biolab.Types (
    MType,
    MesTypeCorrectionVals,
    Label,
    ExpId,
    PlateId,
    Well(..),
    ColonyId(..),
    Measurement(..),
    ExpData,
    colonyId,
)
where

import Data.Map
import Data.DateTime (DateTime)

type MType = String
type Label = String
type ExpId = String
type PlateId = Int
type MesTypeCorrectionVals = Map MType Double

data Well = Well { wRow :: Char , wColumn :: Int } deriving (Eq, Show, Read, Ord)
data ColonyId = ColonyId { cExp :: ExpId, cPlate :: Int, cWell :: Well } deriving (Eq, Show, Ord)
data Measurement = Measurement { mExpDesc :: ExpId, mPlate :: PlateId, mTime :: DateTime, mType :: MType, mWell :: Well, mLabel :: Label, mVal :: Double } deriving (Eq, Show)

colonyId :: Measurement -> ColonyId
colonyId (Measurement { mExpDesc = ed, mPlate = mp, mWell = mw }) = ColonyId { cExp = ed, cPlate = mp, cWell = mw }

type ExpData = Map Label (Map ColonyId [Measurement]) -- experiment data is mapped like this.
