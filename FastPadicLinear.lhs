> module FastPadicLinear where
> import Metric
> import PadicLinear
> import PadicExpansion
> import Data.List
>

nfurcate specialises an integer one place further. e.g. if you have 1*7^2 + 4*7 + 2 it will return 
{ n * 7^3 + 1*7^2 + 4*7 + 2 | n <- (0..7) }. The tricky part is if the number is negative already, then you
want to specialise with further negative digits.

> data PurePower = PurePower { base :: Integer, power :: Int }
> instance Show PurePower
>   where show (PurePower {base=_, power=0}) = "1"
>         show (PurePower {base=b, power=1}) = show b
>         show (PurePower {base=b, power=p}) = (show b) ++ "^" ++ (show p)
>


> nfurcate :: Integer -> PurePower -> [Integer]
> nfurcate numberToGrow (PurePower {base=modulus, power=exponent})
>  | numberToGrow > 0 && (numberToGrow > (modulus ^ exponent)) = error "This would replace existing digits"
>  | numberToGrow < 0 && (-numberToGrow > (modulus ^ exponent)) = error "This would replace existing digits"
>  | numberToGrow > 0 = [ n * modulus ^ exponent + numberToGrow | n <- [ 0 .. (modulus - 1) ]]
>  | numberToGrow < 0 = [ n * modulus ^ exponent + numberToGrow | n <- [ -(modulus - 1) .. 0 ]]
>  | numberToGrow == 0 = [ n * modulus ^ exponent + numberToGrow | n <- [ -(modulus - 1) .. (modulus - 1)]]
>
> nfurcateLine :: IntegerLine -> PurePower -> [IntegerLine]
> nfurcateLine (VerticalLine _) _ _ = error "I probably could make a vertical line nfurcate, but not yet"
> nfurcateLine (IntegerLine {gradient=gradient, yintercept=yintercept}) purePower
>  = [IntegerLine {gradient=g, yintercept=h} | g <- nfurcate gradient purePower, h <- nfurcate yintercept purePower ]
>
> nfurcateLines :: [IntegerLine] -> PurePower -> [IntegerLine]
> nfurcateLines lines purePower = concat [nfurcateLine l purePower | l <- lines]
>
> pruneLines :: [IntegerLine] -> [IntegerPoint] -> PurePower -> [IntegerLine]
> pruneLines candidateLines points purePower = winningElements scoringFunction candidateLines
>  where scoringFunction line = countCorrectEndings purePower line points
>
> countCorrectEndings :: PurePower -> IntegerLine -> [IntegerPoint] -> Int
> countCorrectEndings modulus integerline points =
>    length (filterByCorrectModulus modulus integerline points)
>
> filterByCorrectModulus :: PurePower -> IntegerLine -> [IntegerPoint] -> [IntegerPoint]
> filterByCorrectModulus _ _ [] = []
> filterByCorrectModulus _ (VerticalLine _) _ = error "Cannot filter a vertical line"
> filterByCorrectModulus (purePower@(PurePower {base=modulus, power=exponent})) (il@(IntegerLine {gradient=gradient, yintercept=yintercept})) (p@(IntegerPoint {x=x, y=y}):points) 
>   | ((gradient * x + yintercept) `mod` modulo) == (y `mod` modulo) = p:remainder
>   | otherwise = remainder
>  where remainder = filterByCorrectModulus purePower il points
>        modulo = modulus ^ exponent
>

>
> data Nurcado = Nurcado {
>    candidate_line :: IntegerLine,
>    current_modulus :: PurePower,
>    correctly_handled_points :: [IntegerPoint]
> } deriving (Show)
>
> createBasicNurcados :: [IntegerPoint] -> Integer -> [Nurcado]
> createBasicNurcados points modulus =
>   [updateCorrectlyHandledPoints (
>     Nurcado { candidate_line=l,
>              current_modulus=PurePower {base=modulus, power=1},
>              correctly_handled_points = points}
>     )
>    | x <- nub (sort ([lineThrough p1 p2  | p1 <- points, p2 <- points, p1 /= p2 ]))]
>
> updateCorrectlyHandledPoints :: Nurcado -> Nurcado
> updateCorrectlyHandledPoints (Nurcado {candidate_line=cl, current_modulus=cm, correctly_handled_points=chp}) =
>   Nurcado { candidate_line=cl,
>             current_modulus=cm,
>             correctly_handled_points=filterByCorrectModulus cm cl chp }
> 

I'm here... converting to Nurcado


> nfurcationAlgorithmStep :: (IntegerLine) -> [IntegerPoint] -> PurePower -> [IntegerLine]
> nfurcationAlgorithmStep currentLine points purePower = 
>   [(line, filterByCorrectModulus purePower line points) | line <- cleanedLines ]
>  where newLines = nfurcateLine currentLine purePower
>        nextLines = pruneLines newLines points purePower
>        cleanedLines = nub (sort nextLines)
>
> nfurcationAlgorithmSteps :: [IntegerLine] -> [IntegerPoint] -> PurePower -> Int -> [(IntegerLine,Double,Int)]
> nfurcationAlgorithmSteps lines points (purePower@(PurePower {base=modulus, power=exponent})) limit 
>  | exponent == limit = finalSummary
>  | exponent < limit = nfurcationAlgorithmSteps cleanedNextCalc modulus (exponent+1) limit
>  | exponent > limit = error "Existing exponent higher than limit"
>  where nextCalc = concat [nfurcationAlgorithmStep l points purePower | l <- lines]
>        best = winningElements length nextCalc -- this line isn't right
>        finalSummary = [(l,sumResidualDistance (Padic modulus) l ps, countCorrectEndings purePower l ps)
>                         | (l,ps) <- cleanedNextCalc]
>
> nfurcationAlgorithm :: [IntegerPoint] -> PurePower -> [(IntegerLine, Double, Int)]
> nfurcationAlgorithm points (PurePower {base=modulus,power=limit}) =
>     nfurcationAlgorithmSteps (initialNfurcationLines points modulus) points startingPower limit
>  where startingPower = PurePower {base=modulus, power=1}


 enumerateEndingCounts :: Integer -> [IntegerPoint] -> [(IntegerLine, Int)]
 enumerateEndingCounts modulus points = [ 
    (line, countCorrectEndings modulus line points) |
     gradTail <- [0 .. (modulus - 1)] ,
     interceptTail <- [0 .. (modulus - 1)]
   ]

 maxBy :: (a -> Int) -> [a] -> a 
 maxBy f elems = last (sortBy c elems)
  where c q q' = compare (f q) (f q')

 findBestEnding :: Integer -> [IntegerPoint] -> (Integer, Integer)
 findBestEnding modulus points = (gradTail, interceptTail)
  where (gradTail, interceptTail, _) = maxBy (\(a,b,c) -> c) (enumerateEndingCounts modulus points)

 rightShift :: Integer -> Integer -> Integer -> IntegerPoint -> IntegerPoint
 rightShift modulus gradTail interceptTail (IntegerPoint {x=x, y=y}) =
  IntegerPoint {x=(x `div` modulus), y=((y + overflow) `div` modulus)}
  where overflow = modulus * (((x `mod` modulus) * gradTail + interceptTail) `div` modulus)

 coefficientTermListStep :: Metric -> [IntegerPoint] -> (Integer, Integer, [IntegerPoint])
 coefficientTermListStep Archimedean _ = error "tailish only works with padic metrics"
 coefficientTermListStep (Padic modulus) points = (gradTail, interceptTail, updatedPoints)
   where (gradTail, interceptTail) = findBestEnding modulus points
         nextStepPoints = filterByCorrectEnding modulus points gradTail interceptTail
         updatedPoints = [rightShift modulus gradTail interceptTail p | p <- nextStepPoints]
 
 nthApproximation :: Metric -> [IntegerPoint] -> Int -> [(Integer, Integer, [IntegerPoint])]
 nthApproximation m points 0 = []
 nthApproximation m points 1 = [coefficientTermListStep m points]
 nthApproximation m points n = (gradTail, interceptTail, updatedPoints) : (nthApproximation m updatedPoints (n-1))
   where (gradTail, interceptTail, updatedPoints) = coefficientTermListStep m points

 nthApproximationLine :: Metric -> [IntegerPoint] -> Int -> IntegerLine
 nthApproximationLine Archimedean _ _ = error "can't handle archidean metrics yet"
 nthApproximationLine (m@(Padic p)) points n = IntegerLine {gradient=gr, yintercept=intc}
  where pz = nthApproximation m points n
        grads = [ g | (g,_,_) <- pz ]
        intercepts = [ i | (_,i,_) <- pz ]
        gr = pToInteger (PadicIntegerExpansion {prime=p, coefficients=grads})
        intc = pToInteger (PadicIntegerExpansion {prime=p, coefficients=intercepts}) 



