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
