> {-# LANGUAGE FlexibleInstances #-}
> module Metric where
> import Data.Ratio
>
> data Metric = Padic Integer | Euclidean
> -- Maybe one day I'll care about manhattan and other more exotic metrics
>
> class Measure a where
>   metric :: Metric -> a -> Double 

This should perhaps be a class with methods
 - metric
 - norm
 - maybe other stuff also (regression tests)

I don't think the Haskell type system is capable of handling
this properly though.

> padic_rational_metric :: Integer -> Rational -> Double
> padic_rational_metric p n 
>  | numerator n == 0 = 0.0
>  | otherwise = (fromInteger p) ** (- (fromInteger expon))
>   where number_of_divisors m 
>          | remainder == 0  = 1 + (number_of_divisors quotient)
>          | otherwise = 0
>          where (quotient, remainder) = m `quotRem` p
>         expon = (number_of_divisors $! (numerator n)) - 
>                    (number_of_divisors $! (denominator n)) 
>
> instance Measure Integer where
>   metric (Padic p) n = padic_rational_metric p (n % 1)
>   metric (Euclidean) n = fromInteger (abs n)
>
> instance Measure (Rational) where
>   metric (Padic p) n = padic_rational_metric p n
>   metric (Euclidean) n = abs (fromRational n)
>
> euclidean = abs
