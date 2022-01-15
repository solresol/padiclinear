> module Concordance where
>
> data Book = Matthew | Mark | Luke | John
>   deriving Show
>  -- I'll add to it later
> data Verse = Verse { book :: Book, chapter :: Int, verse :: Int, text :: String }
> instance Show Verse
>   where show (Verse {book=b, chapter=c, verse=v, text=s }) =
>           (show b) ++ " " ++ (show c) ++ ":" ++ (show v) ++ " " ++ (show s)
> 
> matthew c v t = Verse { book = Matthew, chapter = c, verse = v, text = t }
> mark c v t = Verse { book = Mark, chapter = c, verse = v, text = t }
> luke c v t = Verse { book = Luke, chapter = c, verse = v, text = t }
> john c v t = Verse { book = John, chapter = c, verse = v, text = t }
>
>
> 
> 
> matthew_6_8 = matthew 6 8 "Your Father knows what you need"
> matthew_6_3 = matthew 6 3 "do not let your left hand know"
> matthew_7_11 = matthew 7 11 "If you then, being evil, know how to give good gifts"
> matthew_7_16 = matthew 7 16 "You will know them by their faults"
> 
> matthew_7_23 = matthew 7 23 "I never knew you" -- knowing a person might be different. Need to check the Greek
> matthew_25_26 = matthew 25 26 "You knew that I reap"
> matthew_27_28 = matthew 27 28 "For he knew that"
> mark_1_34 = mark 1 34"They knew who he was"
> john_4_10 = john 4 10 "If you knew the gift of God"
> john_11_42 = john 11 42 "I knew that you always hear me"
>
>
> matthew_26_73 = matthew 26 73 "the way you talk gives you away"
>
> matthew_5_8 = matthew 5 8 "They shall see God"
> matthew_5_16 = matthew 5 16 "They may see your good works"
> matthew_7_15 = matthew 7 15 "You will see clearly"
> matthew_11_4 = matthew 11 4 "What you see"
> matthew_11_7 = matthew 11 7 "What do you go into the wilderness to see?"
> matthew_13_13 = matthew 13 13 "They do not see"
> matthew_13_15 = matthew 13 15 "They would see"
> matthew_27_49 = matthew 27 49 "Let us see"
> luke_9_27 = luke 9 27 "They see the kingdom"
> john_3_36 = john 3 36 "He will not see life"
> matthew_2_2 = matthew 2 2 "We saw his star"
> matthew_2_10 = matthew 2 10 "When they saw the star"
> matthew_2_16 = matthew 2 16 "When Herod saw"
> 
