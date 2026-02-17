{-# LANGUAGE OverloadedStrings #-}

-- Main.hs からの改善点:
-- 1. コマンドライン引数でファイルを受け取る (対話式の putStrLn + getLine を廃止)
--    引数なしなら標準入力から読む、本物の nl と同じ挙動
-- 2. TIO.readFile で最初から Text として読むので String との変換 (packStrings) が不要
-- 3. 空行は番号を振らず空行のまま残す (filter で消してしまわない)
--    本物の nl は空行を消さず、番号だけ振らない
-- 4. T.justifyRight で行番号を6桁右寄せ + タブ区切り (本物の nl の出力に近い)
-- 5. withFile + hGetContents の lazy IO パターンを避けてシンプルに

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  contents <- case args of
    [path] -> TIO.readFile path
    _      -> TIO.getContents
  TIO.putStr $ nl contents

nl :: T.Text -> T.Text
nl = T.unlines . zipWith formatLine [1 ..] . T.lines

formatLine :: Int -> T.Text -> T.Text
formatLine n line
  | T.null line = T.empty
  | otherwise   = T.justifyRight 6 ' ' (tshow n) <> "\t" <> line

tshow :: Int -> T.Text
tshow = T.pack . show
