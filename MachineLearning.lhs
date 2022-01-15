> module MachineLearning where
> import Data.List
> import Data.Maybe
> import NearestNeighbours
> 
> data UntrainedPredictor a b = UntrainedPredictor ([(a,b)] -> TrainedPredictor a b)
> data Explanation = Blackbox | Explanation String
>   deriving Eq
> data SummaryExplanation = BlackboxSet | SummaryExplanation String
>   deriving Eq
> instance Show Explanation
>   where show Blackbox = "(blackbox predictor with no explanation)"
>         show (Explanation s) = s
>
> instance Show SummaryExplanation
>   where show BlackboxSet = "(blackbox predictor applied to many elements independently)"
>         show (SummaryExplanation s) = s
>
> data TrainedPredictor a b = BlackboxPredictor (a -> b)
>                         | ExplainablePredictor (a -> b) (a -> Explanation) ([a] -> SummaryExplanation)
>
> 
> predictingFunction :: TrainedPredictor a b -> (a -> b)
> predictingFunction (BlackboxPredictor f) = f
> predictingFunction (ExplainablePredictor f _ _) = f
>
> fit :: UntrainedPredictor a b -> [(a,b)] -> TrainedPredictor a b
> fit (UntrainedPredictor f) dataset = f dataset
>
> predict :: TrainedPredictor a b -> a -> b
> predict (BlackboxPredictor f) x = f x
> predict (ExplainablePredictor f _ _) x = f x
>
> fitPredict :: UntrainedPredictor a b -> [(a,b)] -> a -> b
> fitPredict predictor dataset sample = predict (fit predictor dataset) sample
>
> fitPredictForScoring :: Eq b => UntrainedPredictor a b -> [(a,b)] -> (a,b) -> (a,b,b,Bool)
> fitPredictForScoring predictor dataset (samplex,sampley) =
>     (samplex,sampley, testy, sampley == testy)
>  where testy = fitPredict predictor dataset samplex
>
> explain :: TrainedPredictor a b -> a -> Explanation
> explain (BlackboxPredictor _) _ = Blackbox
> explain (ExplainablePredictor _ ex _) x = ex x
>
> explainMultiple :: TrainedPredictor a b -> [a] -> SummaryExplanation
> explainMultiple (BlackboxPredictor _) _ = BlackboxSet
> explainMultiple (ExplainablePredictor _ _ ex) xs = ex xs
>
> onefold :: [a] -> [(a,[a])]
> onefold x_y_pairs =
>   [ afold n x_y_pairs | n <- [ 0 .. (len-1) ] ]
>  where len = length (x_y_pairs)
>        afold :: Int -> [a] -> (a,[a])
>        afold n items = fromJust (uncons (take len (drop n (cycle items))))
>
> onefoldCrossValidateResults :: Eq b => UntrainedPredictor a b -> [(a,b)] -> [(a,b,b,Bool)]
> onefoldCrossValidateResults predictor dataset =
>    [fitPredictForScoring predictor dataset' x' |
>          (x', dataset')  <- onefold dataset ]
> 
> onefoldCrossValidateSummary :: Eq b => UntrainedPredictor a b -> [(a,b)] -> Int
> onefoldCrossValidateSummary predictor dataset = sum (map eval2 results)
>   where
>      results = onefoldCrossValidateResults predictor dataset
>      eval2 (_,_,_,False) = 0
>      eval2 (_,_,_,True) = 1
>
> fitPredictExplain :: UntrainedPredictor a b -> [(a,b)] -> a -> (b, Explanation)
> fitPredictExplain predictor dataset sample = (predict fitted sample, explain fitted sample)
>   where fitted = fit predictor dataset
>
> fitPredictExplainMultiple :: UntrainedPredictor a b -> [(a,b)] -> [a] -> ([b], SummaryExplanation)
> fitPredictExplainMultiple predictor dataset samples = ([predict fitted x | x <- samples], explainMultiple fitted samples)
>   where fitted = fit predictor dataset
>
>

> data Solution m a b = CorrectUsing m a b | Incorrect m a b b | UnableToPredict a b
>   deriving (Eq, Show)
> isCorrectSolution (CorrectUsing _ _ _) = True
> isCorrectSolution _ = False
> isIncorrectSolution (Incorrect _ _ _ _) = True
> isIncorrectSolution _ = False
> isUnableToPredictSolution (UnableToPredict _ _) = True
> isUnableToPredictSolution _ = False
> correctMethods :: Eq m => [Solution m a b] -> [m]
> correctMethods ((CorrectUsing method _ _):xs) = nub (method:correctMethods xs)
> correctMethods [] = []
> correctMethods (_:xs) = correctMethods xs
>
> incorrectMethods :: Eq m => [Solution m a b] -> [m]
> incorrectMethods ((Incorrect method _ _ _):xs) = nub (method:incorrectMethods xs)
> incorrectMethods [] = []
> incorrectMethods (_:xs) = incorrectMethods xs
> correctSolutionsUsing :: Eq m => m -> [Solution m a b] -> [(a,b)]
> correctSolutionsUsing method ((CorrectUsing m x y):xs)
>  | m == method = (x,y):correctSolutionsUsing method xs
>  | otherwise = correctSolutionsUsing method xs
> correctSolutionsUsing _ [] = []
> correctSolutionsUsing method (_:xs) = correctSolutionsUsing method xs
> incorrectSolutionsUsing :: Eq m => m -> [Solution m a b] -> [(a,b,b)]
> incorrectSolutionsUsing method ((Incorrect m x y y'):xs)
>  | m == method = (x,y,y'):incorrectSolutionsUsing method xs
>  | otherwise = incorrectSolutionsUsing method xs
> incorrectSolutionsUsing _ [] = []
> incorrectSolutionsUsing method (_:xs) = incorrectSolutionsUsing method xs
> nonpredictables :: [Solution m a b] -> [(a,b)]
> nonpredictables ((UnableToPredict x y):xs) = (x,y):(nonpredictables xs)
> nonpredictables [] = []
> nonpredictables (_:xs) = nonpredictables xs
> 
> 
> 
> type PointEvaluation a b m = [(a,b)] -> a -> b -> Solution m a b
> data SolutionSummaryElement m a b =
>    CorrectSolutionsUsing m [(a,b)]
>  | IncorrectSolutionsUsing m [(a,b,b)]
>  | UnableToPredictSolutions [(a,b)]
>
> 
> instance (Show a, Show b, Show m) => Show (SolutionSummaryElement m a b) where
>   show (CorrectSolutionsUsing method answers) =
>     (show method) ++ "\n SUCCEEDED for " ++ (nicelyFormatAnswers2 answers) ++ "\n"
>   show (IncorrectSolutionsUsing method answers) =
>     (show method) ++ "\n FAILED for " ++ (nicelyFormatAnswers3 answers) ++ "\n"
>   show (UnableToPredictSolutions answers) = "\nCOULD NOT ANSWER for " ++ (nicelyFormatAnswers2 answers) ++ "\n"
>
> nicelyFormatAnswers2 :: (Show a, Show b) => [(a,b)] -> String
> nicelyFormatAnswers2 answers = intercalate "\n" [" " ++ (show x) ++ " -> " ++ (show y) | (x,y) <- answers ]
>
> nicelyFormatAnswers3 :: (Show a, Show b) => [(a,b,b)] -> String
> nicelyFormatAnswers3 answers = intercalate "\n" [" " ++ (show x) ++ " should become " ++ (show y) ++ " but was predicted as " ++ (show y') | (x,y, y') <- answers ]
>  
>
>
>
> globalEvaluation :: Eq m => PointEvaluation a b m -> [(a,b)] -> [SolutionSummaryElement m a b]
> globalEvaluation pointevaluator fullDataset = correct ++ incorrect ++ nonp
>  where folds = onefold fullDataset
>        pointEvaluate ((x,y),trainingset) = pointevaluator trainingset x y
>        pointevaluations = map pointEvaluate folds
>        correctmethods = correctMethods pointevaluations
>        incorrectmethods = incorrectMethods pointevaluations
>        nonp = [UnableToPredictSolutions (nonpredictables pointevaluations)]
>        correct = [CorrectSolutionsUsing m (correctSolutionsUsing m pointevaluations) | m <- correctmethods]
>        incorrect = [IncorrectSolutionsUsing m (incorrectSolutionsUsing m pointevaluations) | m <- incorrectmethods]
>        
