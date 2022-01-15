> module EuclideanLinguistics where
> import Siegel
> import WordEncoding
> import MachineLearning
> import RationalGeometry
> import LocalLearning
> import WordNeighbourhood
> import NearestNeighbours
>
> siegelBasedAlgorithm :: Codec a => a -> UntrainedPredictor String String
> siegelBasedAlgorithm codec = UntrainedPredictor siegelBasedAlgorithm'
>  where
>   siegelBasedAlgorithm' dataset = BlackboxPredictor predictor
>    where
>      numericdataset = wordpairs2points codec dataset
>      bestline = siegel numericdataset
>      predictor testword = answerAsString
>        where testwordAsNumber = word2number codec testword
>              answerAsNumber = yValueAt bestline testwordAsNumber
>              answerAsString = number2word codec answerAsNumber
>
>
> 
> localEuclideanSiegel :: Codec a => NearnessMeasure -> a -> UntrainedPredictor String String
> localEuclideanSiegel nm codec = makeLocalLearner (euclideanWordNeighbourhood nm codec) (siegelBasedAlgorithm codec)
>
> localHybridSiegel :: Codec a => NearnessMeasure -> a -> UntrainedPredictor String String
> localHybridSiegel nm codec = makeLocalLearner (padicWordNeighbourhood nm codec) (siegelBasedAlgorithm codec)
>
> studyUsingSiegel :: Codec a => a -> PointEvaluation String String RationalLine
> studyUsingSiegel codec dataset testword testanswer
>  | answerAsString == "" = UnableToPredict testword testanswer
>  | answerAsString == testanswer = CorrectUsing bestline testword testanswer
>  | otherwise = Incorrect bestline testword testanswer answerAsString
>  where
>   numericdataset = wordpairs2points codec dataset
>   bestline = siegel numericdataset
>   testwordAsNumber = word2number codec testword
>   answerAsNumber = yValueAt bestline testwordAsNumber
>   answerAsString = number2word codec answerAsNumber
>
> localEuclideanStudyUsingSiegel :: Codec a => NearnessMeasure -> a -> PointEvaluation String String RationalLine
> localEuclideanStudyUsingSiegel nm codec = localisePointEvaluator (euclideanWordNeighbourhood nm codec) (studyUsingSiegel codec)
>
>
> localHybridStudyUsingSiegel :: Codec a => NearnessMeasure -> a -> PointEvaluation String String RationalLine
> localHybridStudyUsingSiegel nm codec = localisePointEvaluator (padicWordNeighbourhood nm codec) (studyUsingSiegel codec)
