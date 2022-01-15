> module UnicodeWordEncoding where
> import WordEncoding
> import RationalGeometry
> import Data.Ratio
> import Data.Char
>
> data UnicodePointCodec = UnicodePointCodec Integer
> instance Codec UnicodePointCodec where
>   word2number _ = word2unicodenum
>   number2maybeword _ r
>     | denominator r /= 1 = Nothing
>     | numerator r < 0 = Nothing
>     | otherwise = Just (unicodeInt2word (numerator r))
>   codecConstructor = UnicodePointCodec largestCharEncodingNumber
>   extendVocabulary x _ = x
>
> largestCharEncodingNumber = 1114112
> -- Actually, this is one more than the largest valid char.
>
> unicodePointCodec :: [String] -> UnicodePointCodec
> unicodePointCodec = vocabInitialisedCodec
>
> -- If I only wanted to handle ascii, I would use this.
> -- largestCharEncodingNumber = 256
>
> word2unicodenum :: String -> Rational
> word2unicodenum [] = 0
> word2unicodenum (x:xs) = (word2unicodenum' (x:xs) 0)%1
>  where word2unicodenum' :: String -> Integer -> Integer
>        word2unicodenum' [c] n = (n * largestCharEncodingNumber) + toInteger (ord (c))
>        word2unicodenum' (c:cs) n = word2unicodenum' cs ((n * largestCharEncodingNumber) + toInteger (ord (c)))
>
> unicodenum2word :: Rational -> String
> unicodenum2word r
>  | denominator r /= 1 = "[fractional word " ++ (show r) ++ "]"
>  | numerator r < 0 = "[negative-value word " ++ (show (numerator r)) ++ "]"
>  | otherwise = unicodeInt2word (numerator r)
>

The type of this really should be a Maybe String -- so many integers
don't encode validly to a string.


> unicodeInt2word :: Integer -> String
> unicodeInt2word n
>  | n <= 0 = ""
>  | otherwise = (unicodeInt2word (n `div` largestCharEncodingNumber)) ++ [(chr (fromInteger (n `mod` largestCharEncodingNumber)))]
>


Hopefully these functions are obsolete.


> wordpair2integerPoint :: String -> String -> RationalPoint
> wordpair2integerPoint w1 w2 = RationalPoint {x=word2unicodenum w1, y = word2unicodenum w2}
>
> wordpairs2integerPoints :: [(String, String)] -> [RationalPoint]
> wordpairs2integerPoints [] = []
> wordpairs2integerPoints ((w1,w2):ws) = (wordpair2integerPoint w1 w2) : wordpairs2integerPoints ws
>
> integerPoints2wordpairs :: [RationalPoint] -> [(String, String)]
> integerPoints2wordpairs rps = [(unicodenum2word (x t), unicodenum2word (y t)) | t <- rps]
>
