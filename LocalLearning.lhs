> module LocalLearning where
> import NearestNeighbours
> import MachineLearning
>
> makeLocalLearner :: NeighbourhoodFinder a b -> UntrainedPredictor a b -> UntrainedPredictor a b
> makeLocalLearner (NeighbourhoodFinder nf) (UntrainedPredictor predictor) = UntrainedPredictor predictor'
>  where
>   predictor' dataset = BlackboxPredictor trainedpredictor'
>    where 
>     trainedpredictor' xval = (predictingFunction (predictor localdatapoints)) xval
>      where
>       localdatapoints  = nf dataset xval
> 
>
> localisePointEvaluator :: NeighbourhoodFinder a b -> PointEvaluation a b c -> PointEvaluation a b c
> localisePointEvaluator (NeighbourhoodFinder nf) evaluator = evaluator'
>   where
>     evaluator' dataset x y = evaluator localpoints x y
>        where
>          localpoints = nf dataset x
>
 
 correctPadicLinesUsed :: [(String,String,[(String,String)])] -> [(RationalLine,(String, String))]
 correctPadicLinesUsed [] = []
 correctPadicLinesUsed ((x,y,trainingset):otherfolds)
  | isNothing thisline = remainder
  | isJust thisline = (fromJust thisline, (x, y)) : remainder
  where thisline = correctPadicLineToUse trainingset x y
        remainder = correctLinesUsed otherfolds

