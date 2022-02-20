> module DummyLinguistics where
> import MachineLearning
> import RationalGeometry
> import WordEncoding
> 
> y_equals_x = RationalLine { gradient = 1 :: Rational, yintercept = 0 :: Rational }
>
> dummyAlgorithm :: Codec a => a -> UntrainedPredictor String String
> dummyAlgorithm codec = UntrainedPredictor dummyAlgorithm'
>   where dummyAlgorithm' _ = BlackboxPredictor predictor
>         predictor testword = testword
> 
> studyUsingDummy :: Codec a => a -> PointEvaluation String String RationalLine
> studyUsingDummy codec dataset testword testanswer
>  | testword == testanswer = CorrectUsing y_equals_x testword testanswer
>  | otherwise = Incorrect y_equals_x testword testanswer ("HEY:: " ++ testword)
