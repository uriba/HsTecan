module RoboUtils ( --rename to RoboCommon
    filterBy,
    mean,
)
where

import qualified Data.ListLike as LL
import qualified Statistics.Sample as S
import qualified Data.Vector.Unboxed as U

filterBy :: (Eq b, LL.ListLike full a) => (a -> b) -> b -> full -> full
filterBy f v = LL.filter ((==) v . f)

mean :: [Double] -> Double
mean = S.mean . U.fromList
