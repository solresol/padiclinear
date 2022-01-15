> module Main where
> import WordEncoding
> import UnicodeWordEncoding
> import Data.Text as T
> import Data.Ratio
> import System.IO
> import System.Posix.Internals
> 
> prompt = do
>   ttyish <- c_isatty (fromInteger stdinNumber)
>   if (toInteger ttyish) == 1 then putStr "word> " else return ()
>   hFlush stdout
>   -- hWaitForInput stdin (-1)
>  where stdinNumber = 0 -- I wish I could do this a bit better
> 
> eventLoop = do
>    prompt
>    finished <- isEOF
>    if finished then do
>      return ()
>    else do
>      inputWord <- getLine
>      if inputWord == "" then return () else do
>        putStrLn (answer inputWord)
>      eventLoop
>
> answer :: String -> String
> answer inputWord = output
>  where stringInputWord = T.pack inputWord
>        stripped = (T.strip stringInputWord)
>        strippedString = T.unpack stripped
>        outputNumber = word2number (codecConstructor :: UnicodePointCodec) strippedString
>        outputNumerator = numerator outputNumber
>        output = show outputNumerator
>        
>
> main = do
>   hSetBuffering stdout NoBuffering
>   hSetBuffering stdin NoBuffering
>   eventLoop
>  
