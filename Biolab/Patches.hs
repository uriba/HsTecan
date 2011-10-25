module Biolab.Patches (
    mean,
    has,
    isLegal
)
where

import qualified Statistics.Sample as S
import qualified Data.Vector.Unboxed as U

has :: (Eq a) => [a] -> a -> Bool
has = flip elem

mean :: [Double] -> Double
mean = S.mean . U.fromList

isLegal :: (RealFloat a) => a -> Bool
isLegal x = not $ isInfinite x || isNaN x
