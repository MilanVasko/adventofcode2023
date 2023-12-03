module Day3 where

import Data.Char (isDigit)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Grid (Grid, (!), (!?))
import Grid qualified as G
import Util qualified as U

filePath :: FilePath
filePath = "data/day3.txt"

run :: IO ()
run = U.loadAndRun' filePath (fromMaybe "error" . calculate1) (fromMaybe "error" . calculate2)

calculate1 :: Text -> Maybe Text
calculate1 content = do
    grid <- parseGrid content
    let numbers = findAllNumbers grid
    let numbersWithNeighbourCoords = map (\(coords, text) -> (G.neighbourCoords grid (T.length text) coords, text)) numbers
    let numbersWithNeighbourChars = map (first (map (grid !))) numbersWithNeighbourCoords
    numbersAdjacentToASymbol <- mapM (U.textToInt . snd) $ filter (containsSymbol . fst) numbersWithNeighbourChars
    return $ show $ sum numbersAdjacentToASymbol

calculate2 :: Text -> Maybe Text
calculate2 content = do
    grid <- parseGrid content
    let allStars = findAllStars grid
    let allStarNeighbourCoords = map (G.neighbourCoords grid 1 . fst) allStars
    let numberStarNeighbourCoords = map (mapMaybe (lookupAndKeepCoordsIfNumber grid)) allStarNeighbourCoords
    let deduplicatedNumberCoords = deduplicateNumberCoords grid numberStarNeighbourCoords
    let deduplicatedNumberStrings = lookupWholeNumbersFromTheRight grid deduplicatedNumberCoords
    let exactlyTwoNumberStrings = filter ((==) 2 . length) deduplicatedNumberStrings
    exactlyTwoNumbers <- mapM (mapM U.textToInt) exactlyTwoNumberStrings
    let powers = map product exactlyTwoNumbers
    return $ show $ sum powers

lookupWholeNumbersFromTheRight :: Grid Char -> [[G.Coords]] -> [[Text]]
lookupWholeNumbersFromTheRight = map . (map . lookupWholeNumberFromTheRight T.empty)

lookupWholeNumberFromTheRight :: Text -> Grid Char -> G.Coords -> Text
lookupWholeNumberFromTheRight accum grid coords@(cx, cy) =
    if isDigit c
        then
            if G.areCoordsInBounds grid (cx - 1, cy)
                then lookupWholeNumberFromTheRight (T.cons c accum) grid (cx - 1, cy)
                else T.cons c accum
        else accum
  where
    c = grid ! coords

deduplicateNumberCoords :: Grid Char -> [[G.Coords]] -> [[G.Coords]]
deduplicateNumberCoords = map . deduplicateNumberCoords'

deduplicateNumberCoords' :: Grid Char -> [G.Coords] -> [G.Coords]
deduplicateNumberCoords' grid coords =
    let normalizedCoords = map (findRightmostCoordsOfANumber grid) coords
     in Set.toList $ Set.fromList normalizedCoords

findRightmostCoordsOfANumber :: Grid Char -> G.Coords -> G.Coords
findRightmostCoordsOfANumber grid (cx, cy) =
    if G.areCoordsInBounds grid newCoords && isDigit (grid ! newCoords)
        then findRightmostCoordsOfANumber grid newCoords
        else (cx, cy)
  where
    newCoords = (cx + 1, cy)

lookupAndKeepCoordsIfNumber :: Grid Char -> G.Coords -> Maybe G.Coords
lookupAndKeepCoordsIfNumber grid coords = fmap (const coords) $ find isDigit $ grid !? coords

containsSymbol :: [Char] -> Bool
containsSymbol = any (\c -> not (isDigit c) && c /= '.')

parseGrid :: Text -> Maybe (Grid Char)
parseGrid content = do
    let rows = lines content
    nonEmptyRows <- nonEmpty rows
    colCount <- findColCount nonEmptyRows
    let rowCount = length rows
    Just $ G.fromList rowCount colCount (T.unpack (T.concat rows))

findAllStars :: Grid Char -> [(G.Coords, Char)]
findAllStars = filterGridItemsWithCoords (== '*')

findAllNumbers :: Grid Char -> [(G.Coords, Text)]
findAllNumbers = foldr combinator [] . filterGridItemsWithCoords isDigit
  where
    combinator :: (G.Coords, Char) -> [(G.Coords, Text)] -> [(G.Coords, Text)]
    combinator (curCoords, curChar) [] = [(curCoords, T.singleton curChar)]
    combinator (curCoords@(curX, _), curChar) (prev@((prevX, _), prevNum) : ax) =
        if curX + 1 == prevX
            then (curCoords, T.cons curChar prevNum) : ax
            else (curCoords, T.singleton curChar) : prev : ax

filterGridItemsWithCoords :: (a -> Bool) -> Grid a -> [(G.Coords, a)]
filterGridItemsWithCoords predicate grid =
    V.toList $
        V.filter (\(_, item) -> predicate item) $
            V.imap (\index item -> (G.indexToCoords grid index, item)) grid.items

findColCount :: NonEmpty Text -> Maybe Int
findColCount rows =
    foldr combinator (Just (head rowLengths)) rowLengths
  where
    rowLengths = T.length <$> rows

    combinator :: Int -> Maybe Int -> Maybe Int
    combinator current (Just previous) = if previous == current then Just current else Nothing
    combinator _ Nothing = Nothing
