> {-# LANGUAGE FlexibleInstances #-}
> module NearestNeighbours where
> import RationalGeometry
> import Metric
> import Data.List
>
> data NearnessMeasure = NearestByDistance Double | NearestByCount Int | NoConstraints
>   deriving (Eq,Show)
> data Weighting = EqualWeight | WeightByDistance
> 
> data NeighbourhoodFinder a b = NeighbourhoodFinder ([(a,b)] -> a -> [(a, b)])
>
> class Neighbourable a where
>    location :: Metric -> a -> a -> Double
>
> instance Neighbourable RationalPoint where
>    location m rp1 rp2 = metric m ((x rp1)-(x rp2))
>
> instance Neighbourable Integer where
>    location m i1 i2 = metric m (i1 - i2)
>
> instance Neighbourable Rational where
>    location m i1 i2 = metric m (i1 - i2)
>
> nearestPoints :: Neighbourable a => [(a,b)] -> Metric -> NearnessMeasure -> a -> [(a,b, Double)]
> nearestPoints ts m nm p = takeByNM nm
>  where
>        points_with_distances = [(p', y', location m p p') | (p',y') <- ts]
>        closer a b = compare (thd3 a) (thd3 b)
>        sorted_with_distances = sortBy closer points_with_distances
>        takeByNM (NearestByCount n) = take n sorted_with_distances
>        takeByNM (NearestByDistance d) = takeWhile (\t -> thd3 t <= d) sorted_with_distances
>        takeByNM (NoConstraints) = sorted_with_distances
>        thd3 (_,_,x) = x
>
> 
