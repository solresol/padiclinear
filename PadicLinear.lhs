> module PadicLinear where
> import RationalGeometry
> import Metric
> import PadicExpansion
> import Data.List
> import Data.Ratio
>
> bestLines :: Metric -> [RationalPoint] -> [(RationalLine, Double)]
> bestLines m points = nub sortedGoodLines 
>  where lines = allInterestingLines points
>        scores = [sumResidualDistance m l points | l <- lines]
>        neg2 (_, s1) = -s1
>        allGoodLines = winningElements neg2 (zip lines scores)
>        sortedGoodLines = sort allGoodLines
> 
> findLowest :: Ord b => [a] -> [b] -> (a, b)
> findLowest [] [] = error "Cannot take best of nothing"
> findLowest [] _ = error "Objects were longer than scores"
> findLowest _ [] = error "Scores were longer than objects"
> findLowest [l] [s] = (l,s)
> findLowest (l1:ls) (s1:ss)
>   | s1 < bests = (l1,s1)
>   | otherwise =  (bestl, bests)
>   where (bestl, bests) = findLowest ls ss
>
> winningElements :: Ord b => (a -> b) -> [a] -> [a]
> winningElements _ [] = []
> winningElements scoringFunction (s1:ss) = 
>     winningElements' [s1] (scoringFunction s1) ss
>  where winningElements' accumulator _ [] = accumulator
>        winningElements' accumulator bestScore (r:rs) 
>            | scoringFunction r > bestScore = winningElements' [r] (scoringFunction r) rs
>            | scoringFunction r < bestScore = winningElements' accumulator bestScore rs
>            | scoringFunction r == bestScore = winningElements' (accumulator ++ [r]) bestScore rs
>
>
> sampleData1 = [RationalPoint (1%1) (2%1), RationalPoint (2%1) (4%1), RationalPoint (3%1) (8%1), RationalPoint (3%1) (0%1)]
> sampleData2 = [RationalPoint (1%1) (0%1), RationalPoint (2%1) (4%1), RationalPoint (2%1) (8%1), RationalPoint (3%1) (0%1)]
> sampleData3 = [origin] ++ [RationalPoint (1%1) (x%1) | x <- [0..10] ] ++ [RationalPoint (2%1) (x*2 % 1) | x <- [0..10] ]
>   -- 11-adically, that has 11 different lines of best fit
> sampleLines1 = allInterestingLines sampleData1
> sampleLines2 = allInterestingLines sampleData2







