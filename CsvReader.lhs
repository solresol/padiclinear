> module CsvReader where
> import Data.Maybe
> 
> data CSVRow = CSVRow [String] deriving (Eq, Ord, Show)
> data CSVHeader = CSVHeader [String] deriving (Eq, Ord, Show)
> data CSVFile = CSVFile CSVHeader [CSVRow] deriving (Eq, Ord, Show)
> getColumn :: String -> CSVHeader -> CSVRow -> String
> getColumn heading (CSVHeader (headers)) (CSVRow datarow)
>  | isJust result = fromJust result
>  | otherwise = error ("There is no header " ++ heading ++ " in the heading list " ++ (show headers))
>  where result = lookup heading (zip headers datarow)
> getColumnList :: [String] -> CSVHeader -> CSVRow -> [String]
> getColumnList [] _ _ = []
> getColumnList (h:hs) ch cr = (getColumn h ch cr) : (getColumnList hs ch cr)
> 
> parseCSVline :: String -> Char -> [String]
> parseCSVline content separator
>   | r == "" = [l]
>   | otherwise = l : (parseCSVline (tail r) separator)
>  where (l, r) = span (\t -> t /= separator) content
>
> parseCSVfile :: String -> CSVFile
> parseCSVfile contents = CSVFile heading body
>   where chunkedContent = [ parseCSVline line '\t' | line <- lines contents ]
>         heading = CSVHeader (head chunkedContent)
>         body = [ CSVRow x | x <- tail chunkedContent ]
>
> readCSVfile :: String -> IO CSVFile
> readCSVfile filename = do
>    contents <- readFile filename
>    return (parseCSVfile contents)
