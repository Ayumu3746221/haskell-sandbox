import Data.Maybe (fromJust)
import Text.Read (readMaybe)

data Token = Num Double | Op (Double -> Double -> Double)

calculate :: String -> Double
calculate = eval . map (fromJust . parseToken) . words

parseToken :: String -> Maybe Token
parseToken str = case readMaybe str of
  Just n -> Just (Num n)
  Nothing -> lookup str opList
  where
    opList = [("+", Op (+)), ("-", Op (-)), ("*", Op (*)), ("/", Op (/))]

eval :: [Token] -> Double
eval = head . foldl step []

step :: [Double] -> Token -> [Double]
step stack (Num n) = n : stack
step (x : y : zs) (Op f) = f y x : zs

----------------------------------------------
--                  test                    --
----------------------------------------------

testCases :: [(String, Double)]
testCases =
  [ ("10 20 +", 30.0),
    ("10 2 3 + *", 50.0),
    ("10 2 / 3 -", 2.0),
    ("2 3 4 * +", 14.0),
    ("-5 10 +", 5.0)
  ]

test :: [(String, Double)] -> Bool
test = all (\(input, expected) -> calculate input == expected)