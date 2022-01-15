> module Siegel where
> import RationalGeometry
> import Data.List
> 
> siegel :: [RationalPoint] -> RationalLine
> siegel points = medianLine lines
>  where lines = [medianOfLinesThroughPoint p points | p <- points]
>
> medianLine :: [RationalLine] -> RationalLine
> medianLine lines = (sort lines) !! midpoint
>  where midpoint = (length lines) `div` 2
>
> medianOfLinesThroughPoint :: RationalPoint -> [RationalPoint] -> RationalLine
> medianOfLinesThroughPoint p ps = medianLine lines
>   where lines = [lineThrough p p' | p' <- ps, ((x p') /= (x p))]
>   -- what should we do if you give me a dataset where (x p') = (x p) for all p' ?
>
>
> siegel_test1 = [RationalPoint {x=x', y=y'}
>                     | (x',y') <- [(0,0), (1,1), (2,3), (4,8), (5,10) ]]
> siegel_test2 = [RationalPoint {x=x', y=y'}
>                     | (x',y') <- [(0,0), (1,1), (2,3) ]]
> siegel_test3 = [RationalPoint {x=x', y=y'}
>                     | (x',y') <- [(0,0), (1,1), (2,2) ]]
