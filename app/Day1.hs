module Day1 where

import Data.Char (digitToInt, isDigit)
import Data.Text qualified as T
import Text.Regex.TDFA ((=~))
import Util qualified as U

run :: IO ()
run = U.loadAndRun "data/day1.txt" (calculate calculation1) (calculate calculation2)

calculate :: (Text -> Maybe (Int, Int)) -> Text -> Maybe Int
calculate calculation content = do
    let rawLines = lines content
    let foundNumbers = U.concatInts <<$>> mapM calculation rawLines
    sum <$> foundNumbers

calculation1 :: Text -> Maybe (Int, Int)
calculation1 = findFirstAndLastDigit . T.filter isDigit

calculation2 :: Text -> Maybe (Int, Int)
calculation2 = (U.liftMaybeFromTuple . U.mapTuple looseTextToInt) . (findFirstMatch &&& findLastMatch)

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
    x -> U.textToInt x

findFirstMatch :: Text -> Text
findFirstMatch = flip (=~) needleRegex

findLastMatch :: Text -> Text
findLastMatch = T.reverse . flip (=~) reverseNeedleRegex . T.reverse

findFirstAndLastDigit :: Text -> Maybe (Int, Int)
findFirstAndLastDigit text = case T.uncons text of
    Just (first', rest) -> case T.unsnoc rest of
        Just (_, last') -> Just $ U.mapTuple digitToInt (first', last')
        Nothing -> Just $ U.mapTuple digitToInt (first', first')
    Nothing -> Nothing
