> module Primes where
> import Data.List
> import Data.Maybe
> 
> smallestPrimeBiggerThan :: Integer -> Integer
> smallestPrimeBiggerThan q = head [ p | p <- primesTo (q * 2 + 1) , p > q ]
>
>
> primesTo m = sieve [2..m]
>   where sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
>         sieve [] = []

I need to be a little bit random in my allocation of words to integers for
VocabEnumerationWordEncoding.

I want it repeatable, but if I load in all the singulars and then all
the plurals, I don't want there to be a simple linear relationship
that it can learn.

So I want a sequence of numbers where for all n, x_i -> x_i+n is not linear for many i

This function is good enough for this purpose...

makeNumberAllocation 5 = [3,1,4,2]
makeNumberAllocation 7 = [1,6,5,3,4,2]
makeNumberAllocation 11 = [1,10,8,7,6,9,3,5,4,2]


> makeNumberAllocation :: Integer -> [Integer]
> makeNumberAllocation modulus = nextdo []
>   where set_to_sort = [ 2 .. (modulus - 1) ]
>         next_unused :: [Integer] -> [Integer] -> Maybe Integer
>         next_unused _ [] = Nothing
>         next_unused xs (y:ys)
>           | y `elem` xs = next_unused xs ys
>           | otherwise = Just y
>         generator starter alreadyseen
>           | starter `elem` alreadyseen = alreadyseen
>           | otherwise = generator ((starter * starter) `mod` modulus) (starter:alreadyseen)
>         nextdo alreadyseen
>           | isNothing (next_unused alreadyseen set_to_sort) = alreadyseen
>           | otherwise = nextdo (generator (fromJust (next_unused alreadyseen set_to_sort)) alreadyseen)
>
