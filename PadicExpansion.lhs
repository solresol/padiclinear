> module PadicExpansion where
>
> data PadicIntegerExpansion = PadicIntegerExpansion {
>    prime :: Integer,
>    coefficients :: [Integer]
> }
>
> pToInteger :: PadicIntegerExpansion -> Integer
> pToInteger (PadicIntegerExpansion {prime=prime, coefficients=coefficients}) = sum placeValues
>  where placeValues = [a * b | (a,b) <- powersAndSizes ]
>        powersAndSizes = zip coefficients primePowers
>        primePowers = [prime ^ n | n <- [ 0 .. ]]
>
> pFromInteger :: Integer -> Integer -> PadicIntegerExpansion
> pFromInteger prime number = PadicIntegerExpansion {prime=prime,coefficients=powerDecompose number}
>  where powerDecompose n
>          | n == 0 = []
>          | n < prime = [n]
>          | otherwise = (n `mod` prime) : (powerDecompose (n `div` prime))
>
> powerShow :: Integer -> Integer -> Integer -> String
> powerShow base 0 multiplier = show  multiplier
> powerShow base 1 1 = show base
> powerShow base 1 multiplier = (show multiplier) ++ "*" ++ (show base)
> powerShow base exponent 1 = (show base) ++ "^" ++ (show exponent)
> powerShow base exponent multiplier
>  = (show multiplier) ++ " * " ++ (show base) ++ "^" ++ (show exponent)
>
> showPowers :: Integer -> [(Integer, Integer)] -> [String]
> showPowers _ [] = []
> showPowers base ((exponent, 0):others) = showPowers base others
> showPowers base ((exponent, multiplier):others) = (powerShow base exponent multiplier) : (showPowers base others)
>

I think for padic problems, it's nice to show the output with increasing powers, rather than decreasing
powers as you would for an ordinary display of powers

> showExpansion :: PadicIntegerExpansion -> String
> showExpansion (PadicIntegerExpansion {coefficients=[]}) = "0"
> showExpansion (PadicIntegerExpansion {prime=p, coefficients=cfs}) = joinS parts
>   where enumerated = zip [0..] cfs
>         parts = showPowers p enumerated
>         joinS [s] = s
>         joinS (s:ss) = s ++ " + " ++ (joinS ss)
>
> instance Show PadicIntegerExpansion where
>   show pie = showExpansion pie
>

