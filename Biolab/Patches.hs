module Biolab.Patches (
    mean,
)
where

import qualified Statistics.Sample as S
import qualified Data.Vector.Unboxed as U

mean :: [Double] -> Double
mean = S.mean . U.fromList
