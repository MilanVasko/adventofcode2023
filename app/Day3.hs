module Day3 where

import Data.Char (digitToInt, isDigit)
import Data.Text qualified as T
import Data.Vector qualified as V
import Grid (Grid, (!))
import Grid qualified as G
import Util qualified as U

filePath :: FilePath
filePath = "data/day3.txt"

run :: IO ()
run = U.loadAndRun filePath calculate calculate

calculate :: Text -> Maybe Int
calculate content = do
    grid <- parseGrid content
    let numbers = findAllNumbers grid
    let numbersWithNeighbourCoords = map (\(coords, text) -> (G.neighbourCoords grid (T.length text) coords, text)) numbers
    let numbersWithNeighbourChars = map (first (map (grid !))) numbersWithNeighbourCoords
    numbersAdjacentToASymbol <- mapM (U.textToInt . snd) $ filter (containsSymbol . fst) numbersWithNeighbourChars
    return $ sum numbersAdjacentToASymbol

containsSymbol :: [Char] -> Bool
containsSymbol = any (\c -> not (isDigit c) && c /= '.')

parseGrid :: Text -> Maybe (Grid Char)
parseGrid content = do
    let rows = lines content
    nonEmptyRows <- nonEmpty rows
    colCount <- findColCount nonEmptyRows
    let rowCount = length rows
    Just $ G.fromList rowCount colCount (T.unpack (T.concat rows))

findAllNumbers :: Grid Char -> [(G.Coords, Text)]
findAllNumbers grid =
    foldr combinator [] $
        V.imap (\index item -> (G.indexToCoords grid index, item)) $
            G.items grid
  where
    combinator :: (G.Coords, Char) -> [(G.Coords, Text)] -> [(G.Coords, Text)]
    combinator (curCoords, curChar) [] = [(curCoords, T.singleton curChar) | isDigit curChar]
    combinator (curCoords@(curX, _), curChar) (prev@((prevX, prevY), prevNum) : ax) =
        if isDigit curChar
            then
                if curX + 1 == prevX
                    then (curCoords, T.cons curChar prevNum) : ax
                    else (curCoords, T.singleton curChar) : prev : ax
            else prev : ax

findColCount :: NonEmpty Text -> Maybe Int
findColCount rows =
    foldr combinator (Just (head rowLengths)) rowLengths
  where
    rowLengths = T.length <$> rows

    combinator :: Int -> Maybe Int -> Maybe Int
    combinator current (Just previous) = if previous == current then Just current else Nothing
    combinator _ Nothing = Nothing
