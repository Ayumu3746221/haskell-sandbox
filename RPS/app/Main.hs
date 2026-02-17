module Main where

import RPS

main :: IO ()
main = do
  putStrLn "自分の手を入力してください"
  myHand <- getLine
  putStrLn "相手の手を入力してください"
  opHand <- getLine

  let hand1 = read myHand :: Hand
  let hand2 = read opHand :: Hand

  print $ battle hand1 hand2
