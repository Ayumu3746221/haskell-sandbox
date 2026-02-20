{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  contents <- case args of
    [path] -> TIO.readFile path
    _ -> TIO.getContents
  TIO.putStr contents
