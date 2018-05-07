module Bonus3 where
import Data.List
import Data.Char

------------------------Bonus 3
--Lowercase for Strings
lowercase :: [Char] -> [Char]
lowercase = map toLower
--this type is represent character and number of character in word
type CharCount = (Char,Int)

wordCharCounts :: String -> [CharCount]
wordCharCounts [] = []
wordCharCounts str = foo1 (nub $ lowercase str) (lowercase str)
  where
    --the function countChar is used to find the given character in word
    countChar ::Char -> String ->Int
    countChar character = length . filter (== character)
    --The function foo1 is used to retrieve the elements from the nubbed array
    --and search them in the original word
    foo1 :: String -> String -> [CharCount]
    foo1 [] _ = []
    foo1 _ [] = []
    foo1 (x:xs) y = (x,countChar x y) : foo1 xs y

