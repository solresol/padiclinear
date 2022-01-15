> module WordEncoding where
> import RationalGeometry
> import Data.Maybe
> import Data.List

Reminder to self: rename "word2number" to "string2number" and likewise
for "number2maybeword"

> class Codec a where
>   word2number :: a -> String -> Rational
>   codecConstructor :: a
>   extendVocabulary :: a -> String -> a
>   extendVocabularyMany :: a -> [String] -> a
>   extendVocabularyMany x [] = x
>   extendVocabularyMany x (w:ws) = extendVocabularyMany (extendVocabulary x w) ws
>    -- yeah, maybe some beautiful mondaic thing would have done that more nicely.
>   vocabInitialisedCodec :: [String] -> a
>   vocabInitialisedCodec s = extendVocabularyMany codecConstructor s
>   number2maybeword :: a -> Rational -> Maybe String
>   number2word :: a -> Rational -> String
>   number2word t r
>     | isJust word = fromJust word
>     | otherwise = ""
>    where word = number2maybeword t r
>   
>
>
> wordpair2point :: Codec a => a -> String -> String -> RationalPoint
> wordpair2point codec w1 w2 = RationalPoint {
>         x = word2number codec w1,
>         y = word2number codec w2
>   }
> wordpairs2points :: Codec a => a -> [(String, String)] -> [RationalPoint]
> wordpairs2points _ [] = []
> wordpairs2points codec ((w1,w2):ws) =
>          (wordpair2point codec w1 w2) : wordpairs2points codec ws
>
> firstSensibleAnswer :: Codec a => a -> [Rational] -> String
> firstSensibleAnswer _ [] = ""
> firstSensibleAnswer codec (r:rs)
>  | isJust word = fromJust word
>  | otherwise = firstSensibleAnswer codec rs
>  where word = number2maybeword codec r
>
> indexOfFirstSensibleAnswer :: Codec a => a -> [Rational] -> Maybe Int
> indexOfFirstSensibleAnswer _ [] = Nothing
> indexOfFirstSensibleAnswer codec (r:rs)
>  | isNothing remainder = Nothing
>  | isJust word = Just 0
>  | otherwise = Just (1 + fromJust remainder)
>  where remainder = indexOfFirstSensibleAnswer codec rs
>        word = number2maybeword codec r

>
> data TransformationRule = TransformationRule RationalLine
>   deriving (Eq, Ord)
>
> instance Show TransformationRule where
>   show (TransformationRule r) = show r
>
> toBits :: Integer -> [Integer]
> toBits 0 = [0]
> toBits 1 = [1]
> toBits n
>   |  n `mod` 2 == 1 = (toBits (n `div` 2)) ++ [1]
>   |  otherwise = (toBits (n `div` 2)) ++ [0]
> toOctets :: Integer -> [Integer]
> toOctets n = extras ++ stuff
>  where stuff = toBits n
>        stuffLen = length stuff
>        topByteLength = stuffLen `mod` 8
>        extrasNeeded = if (topByteLength == 0) then 0 else (8 - topByteLength)
>        extras = take extrasNeeded (repeat 0)
> showOctets :: [Integer] -> String
> showOctets [] = ""
> showOctets xs = (showOctets upperbytes) ++ separator ++ (aschars)
>   where lastbyte = reverse (take 8 (reverse xs))
>         aschars :: String
>         aschars = intercalate "" [show b | b <- lastbyte]
>         upperbytes = reverse (drop 8 (reverse xs))
>         separator = if length upperbytes == 0 then "" else ":"
>
> topByte :: Integer -> Int
> topByte n
>  | n < 256 = fromInteger n
>  | otherwise = topByte (n `div` 256)
>
