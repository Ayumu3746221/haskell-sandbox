{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO

main :: IO ()
main = do
  putStrLn "ファイルを指定してください"
  path <- getLine
  withFile path ReadMode $ \handle -> do
    contents <- hGetContents handle
    let lineList = nlCommand $ lines contents
    mapM_ TIO.putStrLn lineList

nlCommand :: [String] -> [T.Text]
nlCommand = lineNumToStr . addLineNum . packStrings

lineNumToStr :: [(Int, T.Text)] -> [T.Text]
lineNumToStr = map (\(i, t) -> tshow i <> " " <> t)

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

addLineNum :: [T.Text] -> [(Int, T.Text)]
addLineNum = zip [1 ..] . filter (not . T.null)

packStrings :: [String] -> [T.Text]
packStrings = map T.pack
