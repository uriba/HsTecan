module Biolab.Patches (
    mean,
    has,
    isLegal,
    mapFst,
    mapSnd,
)
where

import qualified Statistics.Sample as S
import qualified Data.Vector.Unboxed as U

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (x,y) = (x,f y) -- update to a cleaner implementation

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (x,y) = (f x, y) -- update to a cleaner implementation

has :: (Eq a) => [a] -> a -> Bool
has = flip elem

mean :: [Double] -> Double
mean = S.mean . U.fromList

isLegal :: (RealFloat a) => a -> Bool
isLegal x = not $ isInfinite x || isNaN x
