module RoboUtils ( --rename to RoboCommon
    filterBy,
)
where

import qualified Data.ListLike as LL

filterBy :: (Eq b, LL.ListLike full a) => (a -> b) -> b -> full -> full
filterBy f v = LL.filter ((==) v . f)
