> module Main where
> import System.Environment
> import System.Console.GetOpt
> import Data.Maybe
> import Data.List
> import System.Exit
> import PadicLinguistic
> import NearestNeighbours
> import Control.Monad
> import MachineLearning
> import EuclideanLinguistics
> import RationalGeometry
> import CsvReader
> import WordEncoding
> import UnicodeWordEncoding
> import VocabEnumerationWordEncoding
>
> data OutputOptions = ShowAnswers | ShowRuleFailures | ShowRules deriving (Eq, Show)
>
> data AlgorithmChoice = VocabularyBasedPadic
>                      | VocabularyBasedSiegel
>                      | GlobalPadicLinear
>                      | GlobalSiegel
>                      | LocalPadicLinear Int
>                      | LocalEuclideanSiegel Int
>                      | HybridSiegel Int -- padic local cluster, Euclidean Siegel algorithm
>          deriving (Eq, Show)
> 
> data Output = Output [OutputOptions] deriving (Eq, Show)
>
> data CmdlineFlag = Verbose
>                  | AlgorithmChoiceFlag AlgorithmChoice
>                  | OutputOptionFlag OutputOptions
>   deriving (Eq,Show)
>
> defaultAlgorithm = LocalPadicLinear 5
>
> algorithmChoice :: [CmdlineFlag] -> AlgorithmChoice
> algorithmChoice [] = defaultAlgorithm
> algorithmChoice ((AlgorithmChoiceFlag a):_) = a
> algorithmChoice (_:otherflags) = algorithmChoice otherflags
>
> extractShowOptions :: [CmdlineFlag] -> [OutputOptions]
> extractShowOptions [] = []
> extractShowOptions ((OutputOptionFlag out):otherflags) = out : (extractShowOptions otherflags)
> extractShowOptions (_:otherflags) = extractShowOptions otherflags
>
> options :: [OptDescr CmdlineFlag]
> options =
>  [
>    Option ['v']  ["verbose"]   (NoArg Verbose) "lots of logging messages (unimplemented)",
>    Option ['q']  ["vocabulary-based-padic"] (NoArg (AlgorithmChoiceFlag VocabularyBasedPadic)) "use a vocabulary-based encoder and a p-adic linear regression",
>    Option ['t']  ["vocabulary-pased-siegel"] (NoArg (AlgorithmChoiceFlag VocabularyBasedSiegel)) "use a vocabulary-based encoder and a siegel based linear regression",
>    Option ['m']  ["global-padic-linear"] (NoArg (AlgorithmChoiceFlag GlobalPadicLinear)) "use a unicode-based encoder and a p-adic linear regressor",
>    Option ['g']  ["global-siegel"] (NoArg (AlgorithmChoiceFlag GlobalSiegel)) "use a unicode-based encoder and a siegal-based linear regressor",
>    Option ['p']  ["local-padic"] (ReqArg (\x -> AlgorithmChoiceFlag (LocalPadicLinear (read x))) "COUNT") "a p-adic linear regressor based on the p-adically nearest COUNT points",
>    Option ['s']  ["local-siegel"] (ReqArg (\x -> AlgorithmChoiceFlag (LocalEuclideanSiegel (read x))) "COUNT") "a siegel-based linear regressor based on the euclideanly nearest COUNT points",
>    Option ['y']  ["hybrid-siegel"] (ReqArg (\x -> AlgorithmChoiceFlag (HybridSiegel (read x))) "COUNT") "a Euclidean-metric siegel-based linear regressor based on the p-adically nearest COUNT points",
>    Option ['a']  ["show-answers"] (NoArg (OutputOptionFlag ShowAnswers)) "Show the constructed plurals",
>    Option ['r']  ["show-rule-summary"] (NoArg (OutputOptionFlag ShowRules)) "Show the rules that were used",
>    Option ['u']  ["show-failed-rule-detail"] (NoArg (OutputOptionFlag ShowRuleFailures)) "Show the rules that were used in detail and with examples of failures"
>  ]
>
>
> -- I don't think this is used
> nearnessMeasureToUse :: AlgorithmChoice -> NearnessMeasure
> nearnessMeasureToUse VocabularyBasedPadic = NoConstraints
> nearnessMeasureToUse VocabularyBasedSiegel = NoConstraints
> nearnessMeasureToUse GlobalPadicLinear = NoConstraints
> nearnessMeasureToUse GlobalSiegel = NoConstraints
> nearnessMeasureToUse (LocalPadicLinear m) = NearestByCount m
> nearnessMeasureToUse (LocalEuclideanSiegel m)  = NearestByCount m
> nearnessMeasureToUse (HybridSiegel m)  = NearestByCount m
>
> predictorToUse :: AlgorithmChoice -> [String] -> UntrainedPredictor String String
> predictorToUse VocabularyBasedPadic s = padicBruteForceAlgorithm (vocabEnumeration s)
> predictorToUse VocabularyBasedSiegel s = siegelBasedAlgorithm (vocabEnumeration s)
> predictorToUse GlobalPadicLinear s = padicBruteForceAlgorithm (unicodePointCodec s)
> predictorToUse GlobalSiegel s = siegelBasedAlgorithm (unicodePointCodec s)
> predictorToUse (LocalPadicLinear m) s = padicLocalBruteForceAlgorithm (NearestByCount m) (unicodePointCodec s)
> predictorToUse (LocalEuclideanSiegel m) s = localEuclideanSiegel (NearestByCount m) (unicodePointCodec s)
> predictorToUse (HybridSiegel m) s = localHybridSiegel (NearestByCount m) (unicodePointCodec s)
>
> studyToUse :: AlgorithmChoice -> [String] -> PointEvaluation String String RationalLine
> studyToUse VocabularyBasedPadic s = studyUsingPadicLine (vocabEnumeration s)
> studyToUse VocabularyBasedSiegel s = studyUsingSiegel (vocabEnumeration s)
> studyToUse GlobalPadicLinear s = studyUsingPadicLine (unicodePointCodec s)
> studyToUse GlobalSiegel s = studyUsingSiegel (unicodePointCodec s)
> studyToUse (LocalPadicLinear m) s = localStudyUsingPadicLine (NearestByCount m) (unicodePointCodec s)
> studyToUse (LocalEuclideanSiegel m) s = localEuclideanStudyUsingSiegel (NearestByCount m) (unicodePointCodec s)
> studyToUse (HybridSiegel m) s = localHybridStudyUsingSiegel (NearestByCount m) (unicodePointCodec s)
>

> type SingularsAndPlurals = [(String, String)]
>
> getSingularsAndPlurals :: CSVFile -> SingularsAndPlurals
> getSingularsAndPlurals (CSVFile headers body) = removeAntinomy (zip singulars plurals)
>  where
>   singulars = [dehyphenate (getColumn "singular" headers x) | x <- body]
>   plurals = [dehyphenate (getColumn "plural" headers x) | x <- body]
>
> dehyphenate :: String -> String
> dehyphenate "" = ""
> dehyphenate ('-':xs) = ' ':(dehyphenate xs)
> dehyphenate (x:xs) = x:(dehyphenate xs)
>
> totalVocab :: CSVFile -> [String]
> totalVocab csvfile = nub (singulars ++ plurals)
>   where (singulars, plurals) = unzip (getSingularsAndPlurals csvfile)
>
> removeAntinomy :: [(String, String)] -> [(String, String)]
> removeAntinomy pairs = [head x | x <- xgroups, universallyEqual x]
>  where
>   grouper r s = fst r == fst s
>   xgroups = groupBy grouper (sort pairs)
>   universallyEqual [] = True
>   universallyEqual [_] = True
>   universallyEqual ((x1,y1):(x2,y2):xs) = x1 == x2 && y1 == y2 && (universallyEqual ((x2,y2):xs))
> 
> scoreOurselves :: CSVFile -> AlgorithmChoice -> [(String, String, String, Bool)]
> scoreOurselves csvfile algo = onefoldCrossValidateResults (predictorToUse algo vocab) sps
>  where sps = getSingularsAndPlurals csvfile
>        vocab = totalVocab csvfile
>        
>
> showRawRulesUsedWithFailures :: CSVFile -> AlgorithmChoice -> String
> showRawRulesUsedWithFailures csvfile algo = show (globalEvaluation (studyToUse algo vocab) sps)
>  where sps = getSingularsAndPlurals csvfile
>        vocab = totalVocab csvfile
>        
> showDetailedRulesUsed :: CSVFile -> AlgorithmChoice -> String
> showDetailedRulesUsed csvfile algo = (show algo) ++ "\n" ++ intercalate "\n" (successCheck globalEval 1)
>   where sps = getSingularsAndPlurals csvfile
>         globalEval = globalEvaluation (studyToUse algo vocab) sps
>         successCheck :: [SolutionSummaryElement RationalLine String String] -> Int -> [String]
>         successCheck ((CorrectSolutionsUsing l ans):cs) n
>           | l == line_y_equals_x = ("Rule: keep unchanged worked " ++ show (length ans) ++ " times. Examples: " ++ showSimple) : successCheck cs n
>           | otherwise = ("Rule " ++ (show n) ++ ": worked " ++ show (length ans) ++ " times. Examples: " ++ (showComplex)) : successCheck cs (n+1)
>           where showSimple = intercalate "; " [x | (x,y) <- ans]
>                 showComplex = intercalate "; " [x ++ " -> " ++ y | (x,y) <- ans]
>         successCheck [] _ = []
>         successCheck (_:cs) n = successCheck cs n
>         vocab = totalVocab csvfile 
>
> showScoring :: [(String, String, String, Bool)] -> IO ()
> showScoring [] = putStrLn ""
> showScoring ((singular, plural, predicted, True):rs) = do
>   putStrLn ("Correctly recreated " ++ singular ++ " -> " ++ plural)
>   showScoring rs
> showScoring ((singular, plural, predicted, False):rs) = do
>   putStrLn ("Incorrectly handled " ++ singular ++ " -> " ++ plural ++ " said " ++ predicted ++ " instead")
>   showScoring rs
>
> scoringSummary :: AlgorithmChoice -> [(String, String, String, Bool)] -> String
> scoringSummary algo scores = (show algo) ++ ": " ++ (show right) ++ " right, " ++ (show wrong) ++ " wrong, out of a total of " ++ (show total)
>   where (right, wrong, total) = summary' scores (0,0,0)
>         summary' [] (right, wrong, total) = (right, wrong, total)
>         summary' ((_,_,_,False):xs) (right, wrong, total) = summary' xs (right, wrong+1, total+1)
>         summary' ((_,_,_,True):xs) (right, wrong, total) = summary' xs (right+1, wrong, total+1) 
> 
> 
>
> processFile :: String -> AlgorithmChoice -> [OutputOptions] -> IO ()
> processFile filename algo showoptions = do
>    csvfile <- readCSVfile filename
>    when (ShowAnswers `elem` showoptions) (showScoring (scoreOurselves csvfile algo))
>    when (ShowRuleFailures `elem` showoptions) (putStrLn (showRawRulesUsedWithFailures csvfile algo))
>    when (ShowRules `elem` showoptions) (putStrLn (showDetailedRulesUsed csvfile algo))
>    putStrLn (scoringSummary algo (scoreOurselves csvfile algo))
>
> handleCmdLine :: ([CmdlineFlag], [String], [String]) -> IO ()
> handleCmdLine (flags,filenames,errors)
>   | errors /= [] = ioError (userError (concat errors ++ usageInfo header options))
>   | length filenames /= 1 = ioError (userError ("Incorrect number of filenames passed\n" ++ (usageInfo header options)))
>   | otherwise = processFile (head filenames) algo showoptions
>  where header = "Usage: singular2plural [--verbose] [ALGORITHM] [SHOWOPTIONS] csvfile"
>        algo = algorithmChoice flags
>        showoptions = extractShowOptions flags
>
> main = do
>   argv <- getArgs
>   handleCmdLine (getOpt RequireOrder options argv)
