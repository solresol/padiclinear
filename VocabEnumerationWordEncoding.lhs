> module VocabEnumerationWordEncoding where
> import WordEncoding
> import RationalGeometry
> import Data.Ratio
> import Data.Maybe
> import Data.List
> import Primes
>
> -- I could make this a lot more efficient (e.g. with a pair of hashtables)
> -- but 
> data VocabularyEnumeration = VocabEnumeration {
>         vocab :: [(Integer, String)],
>         vocabModulus :: Integer,
>         unallocatedNumbers :: [Integer]
> }
>   deriving Show
>
> initialDefaultVocabSize = 7
> 
> instance Codec VocabularyEnumeration where
>   codecConstructor = makeVocabEnumeration initialDefaultVocabSize
>   extendVocabulary oldvocab newword = vocabularyExtension oldvocab (words newword)
>   word2number ve word = (phraseEncoding ve (words word)) % 1
>   number2maybeword ve num
>     | numerator num < 0 = Nothing
>     | denominator num /= 1 = Nothing
>     | otherwise = Just ( unwords (phraseDecoding ve (numerator num)))
>
> makeVocabEnumeration :: Integer -> VocabularyEnumeration
> makeVocabEnumeration size =
>    VocabEnumeration {  vocab=[], vocabModulus = size , unallocatedNumbers = makeNumberAllocation size }
>
> remodularise :: VocabularyEnumeration -> VocabularyEnumeration
> remodularise ve = extendVocabularyMany emptyvocab extractedvocab
>   where newvocabsize = smallestPrimeBiggerThan (2 * (vocabModulus ve))
>         emptyvocab = makeVocabEnumeration newvocabsize
>         extractedvocab = map snd (vocab ve)
> 
> vocabularyExtension :: VocabularyEnumeration -> [String] -> VocabularyEnumeration
> vocabularyExtension ve [] = ve
> vocabularyExtension ve (word:otherwords)
>   | wordIsKnown ve word = vocabularyExtension ve otherwords
>   | unallocatedNumbers ve == [] = vocabularyExtension (remodularise ve) (word:otherwords)
>   | otherwise =  vocabularyExtension (
>                    VocabEnumeration { vocab = (head (unallocatedNumbers ve),word):(vocab ve),
>                                       vocabModulus = vocabModulus ve,
>                                       unallocatedNumbers = tail (unallocatedNumbers ve)
>                                     }) otherwords
> -- To-do: I need to randomise the vocabulary and/or just hash them. Otherwise it might just learn that
> -- the plurals are in a certain sequence relative to the singulars
>
> wordIsKnown :: VocabularyEnumeration -> String -> Bool
> wordIsKnown ve word = isJust (findWordInVocab ve word)
>
> vocabEnumeration :: [String] -> VocabularyEnumeration
> vocabEnumeration = vocabInitialisedCodec
>
> findWordInVocab :: VocabularyEnumeration -> String -> Maybe Integer
> findWordInVocab ve word
>  | ' ' `elem` word = error "findWordInVocab won't work if there are spaces in the word"
>  | otherwise = word `lookup` flippedVocab
>   where flippedVocab = [(w,n) | (n,w) <- (vocab ve)]
>
> phraseEncoding :: VocabularyEnumeration -> [String] -> Integer
> phraseEncoding _ [] = 0
> phraseEncoding ve (w:ws)
>   | isJust thisWord = shiftedRemainder + fromJust thisWord
>   | otherwise = error ("Cannot encode " ++ w ++ " in vocabulary: " ++ (show ve))
>  where remainder = phraseEncoding ve ws
>        shiftedRemainder = (vocabModulus ve) * remainder
>        thisWord = findWordInVocab ve w
>
> phraseDecoding :: VocabularyEnumeration -> Integer -> [String]
> phraseDecoding ve n
>  | n < 0 = []
>  | n == 0 = []
>  | isNothing footerWord = []
>  | otherwise = (fromJust footerWord) : (phraseDecoding ve header)
>  where footer = n `mod` (vocabModulus ve)
>        header = n `div` (vocabModulus ve)
>        footerWord = footer `lookup` (vocab ve)
>


