module RPS where

data Hand = Rock | Paper | Scissors deriving (Show, Read, Eq)
data Result = Win | Draw | Lose deriving (Show)

battle :: Hand -> Hand -> Result
battle Rock Scissors = Win
battle Paper Rock = Win
battle Scissors Paper = Win
battle h1 h2
  | h1 == h2 = Draw
  | otherwise = Lose
