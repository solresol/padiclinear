> module PadicLinguistic where
> import PadicLinear
> import RationalGeometry
> import NearestNeighbours
> import Metric
> import Data.List
> import Data.Maybe
> import WordEncoding
> import WordNeighbourhood
> import Data.Ratio
> import MachineLearning
> import LocalLearning
>
> suffixTransformations :: Codec a => a -> [(String, String)] -> [(RationalLine, Double)]
> suffixTransformations codec ws = bestLines (Padic 2) ips
>   where ips = wordpairs2points codec ws
>  -- we need to fix this to support other values of p (instead of just Padic 2)
>  -- This might make a difference when we are dealing with vocabulary-based analysis
>
> 

This next function is not used for anything, but seems useful.

 padicWordNeighbours :: [String] -> String -> String
 padicWordNeighbours corpus needle = unicodenum2word (fst3 (head l))
   where l = nearestPoints [(word2unicodenum x, Nothing) | x <- corpus] (Padic 2) (NearestByCount 1) (word2unicodenum needle)
         fst3 (x,_,_) = x

 algorithm2 :: NearnessMeasure -> [(String, String)] -> String -> ([(String, TransformationRule)],  [(String,String)])
 algorithm2 nm trainingForms testWord = ([(applyRule t testWord, t) | t <- transformRules], ratpoints)
   where ratpoints = padicWordNeighbourhood nm trainingForms testWord
         -- ratpoints = [(unicodenum2word x, unicodenum2word y) | (x,y,_) <- l]
         transforms = suffixTransformations ratpoints
         transformRules = [TransformationRule (fst t) | t <- transforms]
   

 applyRule :: TransformationRule -> String -> String
 applyRule (TransformationRule l) w = unicodenum2word (yValueAt l z)
   where z = word2unicodenum w

 algorithm2testForm :: NearnessMeasure -> [(String, String)] -> String -> String
 algorithm2testForm nm trainingForms testWord = firstSensibleAnswer possibleAnswers
   where ratpoints = padicWordNeighbourhood nm trainingForms testWord
         transforms = suffixTransformations ratpoints
         rationalLines = [fst t | t <- transforms]
         possibleAnswers = [yValueAt l (word2unicodenum testWord) | l <- rationaLines]

> padicBruteForceAlgorithm :: Codec a => a -> UntrainedPredictor String String
> padicBruteForceAlgorithm codec = UntrainedPredictor bruteForceAlgorithm'
>  where
>    bruteForceAlgorithm' dataset = BlackboxPredictor predictor
>     where
>        transforms = suffixTransformations codec dataset
>        rationalLines = [fst t | t <- transforms]
>        predictor testWord = firstSensibleAnswer codec possibleAnswers
>          where
>            possibleAnswers = [yValueAt l (word2number codec testWord) | l <- rationalLines]
>
> padicLocalBruteForceAlgorithm :: Codec a => NearnessMeasure -> a -> UntrainedPredictor String String
> padicLocalBruteForceAlgorithm nm codec = makeLocalLearner (padicWordNeighbourhood nm codec) (padicBruteForceAlgorithm codec)
>
>
> studyUsingPadicLine :: Codec a => a -> PointEvaluation String String RationalLine
> studyUsingPadicLine codec dataset testword testAnswer
>  | isJust marking = CorrectUsing (rationalLines !! (fromJust marking)) testword testAnswer
>  | isNothing bestAnswerPos = UnableToPredict testword testAnswer
>  | otherwise = Incorrect (rationalLines !! (fromJust bestAnswerPos)) testword testAnswer bestAnswer
>   where
>    transforms = suffixTransformations codec dataset
>    rationalLines = [fst t | t <- transforms]
>    testnum = word2number codec testword
>    possibleAnswers = [yValueAt l testnum | l <- rationalLines]
>    possibleAnswersAsWords = map (number2word codec) possibleAnswers
>    marking = elemIndex testAnswer possibleAnswersAsWords
>    bestAnswer = firstSensibleAnswer codec possibleAnswers
>    bestAnswerPos = indexOfFirstSensibleAnswer codec possibleAnswers
> 
> localStudyUsingPadicLine :: Codec a => NearnessMeasure -> a -> PointEvaluation String String RationalLine
> localStudyUsingPadicLine nm codec = localisePointEvaluator (padicWordNeighbourhood nm codec) (studyUsingPadicLine codec)
