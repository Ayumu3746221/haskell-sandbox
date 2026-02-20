{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  content <- case args of
    [path] -> TIO.readFile path
    _ -> TIO.getContents
  TIO.putStr $ myHead content 10

myHead :: T.Text -> Int -> T.Text
myHead text n = T.unlines . takeLine n $ T.lines text

takeLine :: Int -> [T.Text] -> [T.Text]
takeLine _ [] = []
takeLine n (t : ts)
  | n <= 0 = []
  | otherwise = t : takeLine (n - 1) ts
