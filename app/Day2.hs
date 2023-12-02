module Day2 where

import Data.Text qualified as T
import Util qualified as U

data Color = Red | Green | Blue
    deriving stock (Show)

data Cubes = Cubes
    { count :: Int
    , color :: Color
    }
    deriving stock (Show)

newtype Round = Round
    { cubes :: [Cubes]
    }
    deriving stock (Show)

data Game = Game
    { gameId :: Int
    , rounds :: [Round]
    }
    deriving stock (Show)

data ColorCounts = ColorCounts
    { red :: Int
    , green :: Int
    , blue :: Int
    }
    deriving stock (Show)

run :: IO ()
run = U.loadAndRun "data/day2.txt" (calculate calculation1) (calculate calculation2)

calculate :: ([Game] -> [Int]) -> Text -> Maybe Int
calculate calculation = fmap (sum . calculation) . mapM parseGame . lines

calculation1 :: [Game] -> [Int]
calculation1 = map gameId . filter (isGamePossible $ ColorCounts{red = 12, green = 13, blue = 14})

calculation2 :: [Game] -> [Int]
calculation2 = map calculateGamePower

calculateGamePower :: Game -> Int
calculateGamePower game =
    let cubesInAllRounds = concatMap cubes game.rounds
        updateMaximums current accum =
            ColorCounts
                { red = max (chooseCountByColor Red accum) (fromMaybe 0 (getCountIfColorMatches Red current))
                , green = max (chooseCountByColor Green accum) (fromMaybe 0 (getCountIfColorMatches Green current))
                , blue = max (chooseCountByColor Blue accum) (fromMaybe 0 (getCountIfColorMatches Blue current))
                }
        maximums = foldr updateMaximums (ColorCounts{red = 0, green = 0, blue = 0}) cubesInAllRounds
     in maximums.red * maximums.green * maximums.blue

getCountIfColorMatches :: Color -> Cubes -> Maybe Int
getCountIfColorMatches Red Cubes{color = Red, count} = Just count
getCountIfColorMatches Green Cubes{color = Green, count} = Just count
getCountIfColorMatches Blue Cubes{color = Blue, count} = Just count
getCountIfColorMatches _ _ = Nothing

isGamePossible :: ColorCounts -> Game -> Bool
isGamePossible maxCounts game =
    let cubesInAllRounds = concatMap cubes game.rounds
     in all isCountOK cubesInAllRounds
  where
    isCountOK :: Cubes -> Bool
    isCountOK Cubes{count, color} = count <= chooseCountByColor color maxCounts

chooseCountByColor :: Color -> ColorCounts -> Int
chooseCountByColor Red counts = counts.red
chooseCountByColor Green counts = counts.green
chooseCountByColor Blue counts = counts.blue

parseGame :: Text -> Maybe Game
parseGame = parseLineHalves . T.splitOn ":"

parseLineHalves :: [Text] -> Maybe Game
parseLineHalves [firstHalf, secondHalf] = do
    gameId' <- parseId firstHalf
    rounds' <- parseRounds secondHalf
    Just Game{gameId = gameId', rounds = rounds'}
parseLineHalves _ = Nothing

parseId :: Text -> Maybe Int
parseId x = case T.splitOn " " x of
    ["Game", b] -> U.textToInt b
    _ -> Nothing

parseRounds :: Text -> Maybe [Round]
parseRounds = mapM parseRound . T.splitOn ";"

parseRound :: Text -> Maybe Round
parseRound = fmap createRound . mapM (parseCubes . T.strip) . T.splitOn ","
  where
    createRound :: [Cubes] -> Round
    createRound cubes' = Round{cubes = cubes'}

parseCubes :: Text -> Maybe Cubes
parseCubes = parseCubes' . map T.strip . T.splitOn " "
  where
    parseCubes' :: [Text] -> Maybe Cubes
    parseCubes' [rawCount, rawColor] = do
        count' <- U.textToInt rawCount
        color' <- parseColor rawColor
        Just Cubes{count = count', color = color'}
    parseCubes' _ = Nothing

parseColor :: Text -> Maybe Color
parseColor "blue" = Just Blue
parseColor "green" = Just Green
parseColor "red" = Just Red
parseColor _ = Nothing
