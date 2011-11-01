{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Biolab.Utils.Vector (
    -- Generic functions and types:
    Point,
    Series,
    expFitWindow,
    FitData(..),
    range,
    ranges,
    intersectingRanges,
    vxMap,
    subRange,
)
where
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Sample as S
import Statistics.LinearRegression (linearRegressionRSqr)
import Data.Maybe (fromMaybe)
import Control.Applicative (pure)
import Biolab.Patches (mapFst, mapSnd)
-- generic vector utils

vyMap :: (G.Vector v (c,a), G.Vector v (c,b)) => (a -> b) -> v (c,a) -> v (c,b)
vyMap f = G.map (mapSnd f)

vxMap :: (G.Vector v (a,c), G.Vector v (b,c)) => (a -> b) -> v (a,c) -> v (b,c)
vxMap f = G.map (mapFst f)

tails :: (G.Vector v a) => v a -> [v a]
tails v 
    | G.null v = [G.empty]
    | otherwise = v : tails (G.tail v)

slidingWindow :: (G.Vector v a) =>  Int -> v a -> [v a]
slidingWindow span = slidingWindowBy (G.take span)

slidingWindowBy :: (G.Vector v a) =>  (v a -> v a) -> v a -> [v a]
slidingWindowBy f = map f . init . tails

wMap :: (G.Vector v a, G.Vector u b) => (v a -> b) -> [v a] -> u b
wMap f = G.fromList . map f

-- generic Sample/Point utils
type Point = (Double,Double)
type Series = U.Vector Point

windowSize = 5

smoothMean :: Maybe Int -> S.Sample -> S.Sample
smoothMean Nothing = smoothMean . Just $ windowSize
smoothMean span = smooth S.mean span

smooth :: (G.Vector v a)  => (v a -> a) -> Maybe Int -> v a -> v a
smooth f Nothing d = smooth f (Just windowSize) d
smooth f (Just span) d = wMap f . slidingWindow span $ d

xSlidingWindow :: (Num a, Ord a, G.Vector v (a,b)) => a -> v (a,b) -> [v (a,b)]
xSlidingWindow span = slidingWindowBy (takeXrange span)
    where
        start_val = fst . G.head
        takeXrange s v = G.takeWhile ((start_val v + s >) . fst) $ v

data FitData = FitData { fdAlpha :: Double, fdBeta :: Double, fdRSqr :: Double}
    deriving Show

fitData :: Series -> (Double,FitData)
fitData s = (start,FitData { fdAlpha = a, fdBeta = b, fdRSqr = rs})
    where
        (b,a,rs) = linearRegressionRSqr xs ys
        xs = G.map ((+) (-start) . fst) s
        ys = G.map snd s
        start = fst . G.head $ s

expFitWindow :: Double -> Series -> [(Double,FitData)]
expFitWindow span = map fitData . xSlidingWindow span . vyMap (logBase 2)

range :: Num a => (a,a) -> a
range (x,y) = y - x

ranges' :: (G.Vector v (b,a)) => (a -> Bool) -> v (b,a) -> [(b,b)]
ranges' f v
    | G.null v = []
    | otherwise = (fst . G.head $ v1, fst . G.last $ v1) : ranges f v2
    where
        (v1,v2) = G.span (f . snd) v

ranges :: (G.Vector v (b,a)) => (a -> Bool) -> v (b,a) -> [(b,b)]
ranges f v
    | G.null v = []
    | otherwise = ranges' f . G.dropWhile (not . f . snd) $ v

intersect :: (Ord a) => (a,a) -> (a,a) -> Maybe (a,a)
intersect (a1,a2) (b1,b2)
    | begin >= end = Nothing
    | otherwise = Just (begin,end)
        where
            begin = max a1 b1 
            end = min a2 b2

intersectingRanges :: (Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
intersectingRanges _ [] = []
intersectingRanges [] _ = []
intersectingRanges (a1:as) (b1:bs)
    | snd a1 < snd b1 = first_plus $ intersectingRanges as (b1:bs)
    | otherwise  = first_plus $ intersectingRanges (a1:as) bs
        where
            first_plus = (++) (fromMaybe [] . fmap pure $ (intersect a1 b1))

subRange :: (Ord a) => (a,a) -> [(a,b)] -> [(a,b)]
subRange (start,end) = dropWhile (\(x,y) -> x < start) . takeWhile (\(x,y) -> x < end)
