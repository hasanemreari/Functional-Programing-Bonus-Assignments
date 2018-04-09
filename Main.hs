module Main where
import Data.Char
data Color = Red | Black deriving (Show,Eq)
data Suit = Clubs | Diamonds | Hearts | Spades deriving Show
data Rank = Num Int | Jack | Queen | King | Ace deriving Show
data Card = Card { suit :: Suit, rank :: Rank } deriving Show
data Move = Draw | Discard Card deriving Show

cardColor :: Card -> Color
cardColor (Card vSuit _) = case vSuit of
                            Clubs -> Black
                            Spades-> Black
                            _ -> Red
                            
cardValue :: Card -> Int
cardValue (Card _ vRank) = case vRank of
                            Num a -> a
                            Ace -> 11
                            Jack -> 10
                            Queen -> 10
                            King -> 10
---------------------Remove function delete all x in array-------------------------
removeCard :: Card -> [Card] -> [Card]
removeCard x (y:ys) = if cardColor x == cardColor y && cardValue x == cardValue y
  then removeCard x ys
  else y : removeCard x ys
  
allSameColor :: [Card] -> Bool
allSameColor xs = case xs of
                    []-> True
                    [_]-> True
                    x1:xs@(x2:_) ->cardColor x1 ==cardColor x2 && allSameColor xs
 
sumCards :: [Card] -> Int
sumCards x
  |otherwise = sumCardsIter 0 x
    where
      sumCardsIter :: Int -> [Card] -> Int
      sumCardsIter acc xs = case xs of
                        []-> acc
                        x1:xs ->sumCardsIter (acc + (cardValue x1)) xs

score :: [Card] -> Int ->Int
score x goal
  | sumCards x > goal && allSameColor x = ((sumCards x)-goal)`div`2
  | sumCards x > goal = ((sumCards x)*3)-goal
  |otherwise =  goal - (sumCards x)

convertSuit :: Char -> Suit
convertSuit c
  | c `elem` "dD" = Diamonds
  | c `elem` "cC" = Clubs
  | c `elem` "hH" = Hearts
  | c `elem` "sS" = Spades
  |otherwise =  error "Suit is unknown"
  
convertRank :: Char -> Rank
convertRank c
  | isDigit c = (Num (digitToInt c))
  | c `elem` "tT" = Num 10
  | c `elem` "aA" = Ace
  | c `elem` "jJ" = Jack
  | c `elem` "qQ" = Queen
  | c `elem` "kK" = King
  |otherwise =  error "Rank is unknown"
runGame :: ()-> [String] -> Int -> Int
runGame = undefined

readMoves :: IO [String]
readMoves = undefined

readCards :: IO ()
readCards = undefined

main :: IO ()
main = do putStrLn "Enter cards:"
          cards <- readCards
          -- putStrLn (show cards)

          putStrLn "Enter moves:"
          moves <- readMoves
          -- putStrLn (show moves)
          putStrLn "Enter goal:"
          line <- getLine
          let goal = read line :: Int
          let score = runGame cards moves goal
          putStrLn ("Score: " ++ show score)
