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

--The function sentenceWordCharCount is used to convert string array to CharCount type array.
sentenceWordCharCount :: [String]-> [CharCount]
sentenceWordCharCount =  wordCharCounts . wordConcat
  where
    --The function wordConcat is used to convert words in a array into a single word.
    wordConcat :: [String]-> String
    wordConcat = foldl1 (++)
--The function dictCharCounts is used to convert given String array
--to String and CharCount type array.
dictCharCounts :: [String] -> [(String,[CharCount])]
dictCharCounts [] = []
dictCharCounts (x:xs) = (x,wordCharCounts x) : dictCharCounts xs

--The function dictWordsByCharCounts finds out whether it is or not in the array,
--but it can not place the same ones in one array and the others in a different single array.
dictWordsByCharCounts :: [(String,[CharCount])] -> [String]
dictWordsByCharCounts (x:xs)
  |elemCharCount (snd x) (snd (head xs)) = [fst x ++ fst (head xs)]
  |otherwise = []
    where
      --The function elemCharCount find the every first array element in second
      --array. If second array contain all element return true. One missing return false.
      elemCharCount :: [CharCount] -> [CharCount] -> Bool
      elemCharCount _ [] = True
      elemCharCount [] _ = True
      elemCharCount (a:as) q = (elem a q) && (elemCharCount as q)
