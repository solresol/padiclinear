> module RationalGeometry where
> import Data.Ratio
> import Data.List
> import Metric
>
> data RationalPoint = RationalPoint { x :: Rational, y :: Rational }
>   deriving (Eq, Ord)
>
> instance Show RationalPoint where
>   show (RationalPoint {x=x,y=y})
>     | denominator x == 1 && denominator y == 1 = "(" ++ (show (numerator x)) ++ "," ++ (show (numerator y)) ++ ")"
>     | denominator x == 1 = "(" ++ (show (numerator x)) ++ "," ++ (show (numerator y)) ++ "/" ++ (show (denominator y)) ++ ")"
>     | denominator y == 1 = "(" ++ (show (numerator x)) ++ "/" ++ (show (denominator x)) ++ "," ++ (show (numerator y)) ++ ")"
>     | otherwise = "(" ++ (show (numerator x)) ++ "/" ++ (show (denominator x)) ++ "," ++ (show (numerator y)) ++ "/" ++ (show (denominator y)) ++ ")"
>
> data RationalLine = RationalLine { gradient :: Rational, yintercept :: Rational }
>                  | VerticalLine { xintercept :: Rational }
>   deriving (Eq, Ord)
>
> line_y_equals_x = RationalLine { gradient = 1, yintercept = 0 }
>
> origin = RationalPoint { x=0 % 1, y=0 % 1}
>
> instance Show RationalLine where
>   show (VerticalLine {xintercept=x}) = "vertical line at x = " ++ (show x)
>   show (RationalLine {gradient=m, yintercept=b})
>      | m == 0 && b == 0 =  "y=0"
>      | m == -1 && b == 0 = "y=-x"
>      | m == 1 && b == 0 = "y=x"
>      | b == 0 && denominator m == 1 = "y=" ++ (show (numerator m)) ++ "x"
>      | b == 0 = "y=" ++ (show (numerator m)) ++ "x/" ++ (show (denominator m))
>      | m == 0 = "y=" ++ (displayYIntercept b)
>      | m == 1 = "y=x" ++ (displayYIntercept b)
>      | m == -1 = "y=-x" ++ (displayYIntercept b)
>      | denominator m == 1 = "y=" ++ (show (numerator m)) ++ "x" ++ (displayYIntercept b)
>      | otherwise = "y=" ++ (show (numerator m)) ++ "x/" ++ (show (denominator b)) ++ (displayYIntercept b)
> 
> displayYIntercept b
>     | b > 0 && denominator b == 1 = "+" ++ (show (numerator b))
>     | b > 0 = "+" ++ (show (numerator b)) ++ "/" ++ (show (denominator b))
>     | b < 0 && denominator b == 1 = "-" ++ (show (0-(numerator b)))
>     | b < 0 = "-" ++ (show (0-(numerator b))) ++ "/" ++ (show (denominator b)) 
>     | b == 0 = ""
>
> data VerticalDifference = Finite Rational | Infinite
>   deriving (Show, Eq, Ord)
>
> lineThrough :: RationalPoint -> RationalPoint -> RationalLine
> lineThrough p1@(RationalPoint x1 y1) p2@(RationalPoint x2 y2)
>   | p1 == p2 = error ("Identical points given: " ++ (show p1))
>   | x1 == x2 = VerticalLine x1
>   | otherwise = RationalLine g y0
>  where rise = y2 - y1
>        run = x2 - x1
>        g = rise / run
>        y0 = y1 - (x1 * g)
>
> yValueAt :: RationalLine -> Rational -> Rational
> yValueAt (VerticalLine x') _ = error "Undefined: yValueAt "
> yValueAt (RationalLine g y0) x = y0 + g * x
>
> yDistanceToLine :: RationalLine -> RationalPoint -> VerticalDifference
> yDistanceToLine (VerticalLine x) (RationalPoint x' _)
>   | x == x' = Finite 0
>   | otherwise = Infinite
> yDistanceToLine (RationalLine g y0) (RationalPoint x y)
>   = Finite (y - (y0 + (g * x)))
>
> allInterestingLines :: [RationalPoint] -> [RationalLine]
> allInterestingLines points = nub (sort ([lineThrough p1 p2 | (p1, p2) <- pointpairs]))
>   where pointpairs =  [(p1,p2) | p1 <- sp, p2 <- sp, p1 < p2, (x p1) /= (x p2)]
>         sp = sort (points)
>
> sumResidualDistance :: Metric -> RationalLine -> [RationalPoint] -> Double
> sumResidualDistance _ (VerticalLine _) _ = error "Can't calculate residuals for a vertical line"
> sumResidualDistance _ _ [] = 0.0
> sumResidualDistance m line ((RationalPoint x' y'):points) = (metric m yhat) `seq` (metric m yhat) + remainder
>   where remainder = (sumResidualDistance m line points)
>         yhat = (y' - yValueAt line x')
>
