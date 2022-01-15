> module Scoring where
> import Data.List
> import Data.Maybe
> import WordNeighbourhood
> import MachineLearning
>
> onefoldCrossValidateResults :: Eq b => UntrainedPredictor a b -> [(a,b)] -> [(a,b,b,Bool)]
>
> ([(a,b)] -> a -> b) -> [(a,b)] -> [(a,b,b,Bool)]
> onefoldCrossValidateResults f pairs = map eval1 (onefold pairs)
>   where
>         eval1 ((testX, testY),train)
>           | y_hat == testY = (testX, testY, y_hat, True)
>           | otherwise = (testX, testY, y_hat, False)
>          where y_hat = f train testX
>
> onefoldCrossValidateSummary :: Eq b => ([(a,b)] -> a -> b) -> [(a,b)] -> Int
> onefoldCrossValidateSummary f pairs = sum (map eval2 results)
>   where
>      results = onefoldCrossValidateResults f pairs
>      eval2 (_,_,_,False) = 0
>      eval2 (_,_,_,True) = 1
>


> type LanguageAlgorithm = [(String, String)] -> String -> String
> scoreLocalRegionThenAlgo :: NeighbourhoodFinder -> LanguageAlgorithm -> [(String,String)] -> [(String, String, String, Bool)]
> scoreLocalRegionThenAlgo nf algo dataset = onefoldCrossValidateResults algo' dataset
>  where algo' trainingset testword
>   localneighbourhood = nf trainingset testword
>   
