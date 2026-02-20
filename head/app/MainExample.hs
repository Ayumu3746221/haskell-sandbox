{-# LANGUAGE OverloadedStrings #-}

-- このファイルは head -n オプション対応のお手本実装です。
-- cabal はこのファイルをビルド対象にしませんが、参考用に残しています。
-- 動かしたい場合は Main.hs の内容をこれに差し替えてください。

module MainExample where

import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- ----------------------------------------------------------------
-- データ型でオプションを表現する
-- ----------------------------------------------------------------

data Options = Options
  { numLines :: Int           -- 表示する行数
  , filePath :: Maybe FilePath -- ファイルパス（Nothing なら stdin）
  }

defaultLines :: Int
defaultLines = 10

-- ----------------------------------------------------------------
-- コマンドライン引数のパース
-- ----------------------------------------------------------------
--
-- 対応パターン:
--   head                  stdin から 10 行
--   head file.txt         ファイルから 10 行
--   head -n 5             stdin から 5 行
--   head -n 5 file.txt    ファイルから 5 行
--
-- ポイント: [String] をパターンマッチでリストとして分解する
--
parseArgs :: [String] -> Either String Options
-- 引数なし
parseArgs [] =
  Right $ Options defaultLines Nothing

-- 引数が1つ
parseArgs [arg]
  -- "-" で始まる引数（オプション）が1つだけ → 不完全または未知のオプション
  | "-" `isPrefixOf` arg = Left $ "unknown or incomplete option: " ++ arg
  -- それ以外はファイルパスとみなす
  | otherwise = Right $ Options defaultLines (Just arg)

-- 引数が2つ以上: 先頭が "-n" かどうか確認
parseArgs ("-n" : nStr : rest) =
  -- reads :: String -> [(Int, String)] で安全に Int へ変換
  case reads nStr of
    [(n, "")] ->
      case rest of
        -- -n 5 (ファイルなし)
        []     -> Right $ Options n Nothing
        -- -n 5 file.txt
        [path] -> Right $ Options n (Just path)
        -- -n 5 foo bar ... (引数が多すぎる)
        _      -> Left "too many arguments"
    -- 数値に変換できなかった場合
    _ -> Left $ "invalid number after -n: " ++ nStr

-- それ以外はすべてエラー
parseArgs args =
  Left $ "invalid arguments: " ++ unwords args

-- ----------------------------------------------------------------
-- エントリポイント
-- ----------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left errMsg -> do
      hPutStrLn stderr $ "head: " ++ errMsg
      hPutStrLn stderr "usage: head [-n lines] [file]"
      exitFailure
    Right opts -> do
      content <- case filePath opts of
        Just path -> TIO.readFile path
        Nothing   -> TIO.getContents
      TIO.putStr $ myHead content (numLines opts)

-- ----------------------------------------------------------------
-- コア処理
-- ----------------------------------------------------------------

-- 先頭 n 行を取り出す
-- 現在の実装の takeLine と同じことを Prelude の take で書ける
myHead :: T.Text -> Int -> T.Text
myHead text n = T.unlines . take n $ T.lines text
