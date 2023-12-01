module Main where

import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Main.Utf8 (withUtf8)
import Relude.Unsafe (fromJust)

main :: IO ()
main = withUtf8 $ do
    content <- decodeUtf8 <$> readFileBS "data/day1.txt"
    let rawLines = lines $ T.pack content
    let numbers = map (T.filter isDigit) rawLines
    let firstAndLast = fromJust $ mapM getFirstAndLastChar numbers
    let finalNumbers = fromJust $ mapM (textToInt . T.pack . charsToList) firstAndLast
    print $ sum finalNumbers

-- TODO: refactor
textToInt :: Text -> Maybe Int
textToInt t = case T.decimal t of
    Right (n, _) -> Just n
    Left _ -> Nothing

getFirstAndLastChar :: Text -> Maybe (Char, Char)
getFirstAndLastChar text = case T.uncons text of
    Just (first', rest) -> case T.unsnoc rest of
        Just (_, last') -> Just (first', last')
        Nothing -> Just (first', first')
    Nothing -> Nothing

charsToList :: (Char, Char) -> [Char]
charsToList (a, b) = [a, b]
