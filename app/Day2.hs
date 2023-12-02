module Day2 where

import Text.Megaparsec (Parsec, choice, chunk, parse, sepBy, sepBy1)
import Text.Megaparsec.Char (char, hspace)
import Text.Megaparsec.Char.Lexer (decimal)
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
parseGame content = rightToMaybe $ parse parser "" content

type Parser = Parsec Void Text

parser :: Parser Game
parser = do
    id' <- gameIdParser
    void (char ':')
    rounds' <- roundParser `sepBy1` char ';'
    return Game{gameId = id', rounds = rounds'}

gameIdParser :: Parser Int
gameIdParser = do
    void (chunk "Game")
    void hspace
    decimal

roundParser :: Parser Round
roundParser = do
    cubes' <- cubesParser `sepBy` char ','
    return Round{cubes = cubes'}

cubesParser :: Parser Cubes
cubesParser = do
    void hspace
    count' <- decimal
    void hspace
    color' <-
        choice
            [ Red <$ chunk "red"
            , Green <$ chunk "green"
            , Blue <$ chunk "blue"
            ]
    return Cubes{count = count', color = color'}
