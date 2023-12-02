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

data MaxCounts = MaxCounts
    { red :: Int
    , green :: Int
    , blue :: Int
    }

run :: IO ()
run = U.loadAndRun "data/day2.txt" calculate calculate

calculate :: Text -> Maybe Int
calculate = fmap (sum . map gameId . filter (isGamePossible $ MaxCounts{red = 12, green = 13, blue = 14})) . mapM parseGame . lines

isGamePossible :: MaxCounts -> Game -> Bool
isGamePossible maxCounts game =
    let cubesInAllRounds = concatMap cubes game.rounds
     in all isCountOK cubesInAllRounds
  where
    isCountOK :: Cubes -> Bool
    isCountOK Cubes{count, color} = count <= chooseMaxCount color

    chooseMaxCount :: Color -> Int
    chooseMaxCount Red = maxCounts.red
    chooseMaxCount Green = maxCounts.green
    chooseMaxCount Blue = maxCounts.blue

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
