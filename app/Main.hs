module Main where

import Data.Char (digitToInt, isDigit)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Main.Utf8 (withUtf8)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = withUtf8 $ do
    content <- T.pack . decodeUtf8 <$> readFileBS "data/day1.txt"
    calculate calculation1 content
    calculate calculation2 content

calculate :: (Text -> Maybe (Int, Int)) -> Text -> IO ()
calculate calculation content = do
    let rawLines = lines content
    let foundNumbers = concatInts <<$>> mapM calculation rawLines
    print $ sum <$> foundNumbers

calculation1 :: Text -> Maybe (Int, Int)
calculation1 = findFirstAndLastDigit . T.filter isDigit

calculation2 :: Text -> Maybe (Int, Int)
calculation2 = (liftMaybeFromTuple . mapTuple looseTextToInt) . (findFirstMatch &&& findLastMatch)

needleRegex :: Text
needleRegex = "one|two|three|four|five|six|seven|eight|nine|[1-9]"

reverseNeedleRegex :: Text
reverseNeedleRegex = "eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|[1-9]"

looseTextToInt :: Text -> Maybe Int
looseTextToInt t = case t of
    "one" -> Just 1
    "two" -> Just 2
    "three" -> Just 3
    "four" -> Just 4
    "five" -> Just 5
    "six" -> Just 6
    "seven" -> Just 7
    "eight" -> Just 8
    "nine" -> Just 9
    x -> textToInt x

findFirstMatch :: Text -> Text
findFirstMatch = flip (=~) needleRegex

findLastMatch :: Text -> Text
findLastMatch = T.reverse . flip (=~) reverseNeedleRegex . T.reverse

textToInt :: Text -> Maybe Int
textToInt t = case T.decimal t of
    Right (n, _) -> Just n
    Left _ -> Nothing

findFirstAndLastDigit :: Text -> Maybe (Int, Int)
findFirstAndLastDigit text = case T.uncons text of
    Just (first', rest) -> case T.unsnoc rest of
        Just (_, last') -> Just $ mapTuple digitToInt (first', last')
        Nothing -> Just $ mapTuple digitToInt (first', first')
    Nothing -> Nothing

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple fn (a, b) = (fn a, fn b)

liftMaybeFromTuple :: (Maybe a, Maybe b) -> Maybe (a, b)
liftMaybeFromTuple (Just a, Just b) = Just (a, b)
liftMaybeFromTuple (_, _) = Nothing

concatInts :: (Int, Int) -> Int
concatInts (a, b) = 10 * a + b
