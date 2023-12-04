module Day4 where

import Data.Set qualified as Set
import Text.Megaparsec (Parsec, chunk, parse, sepBy1, sepEndBy, someTill)
import Text.Megaparsec.Char (char, hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Util qualified as U

data Card = Card
    { cardId :: Int
    , targetNumbers :: [Int]
    , gotNumbers :: [Int]
    }
    deriving stock (Show)

filePath :: FilePath
filePath = "data/day4.txt"

run :: IO ()
run = U.loadAndRun' filePath calculate calculate

calculate :: Text -> Text
calculate = U.prettyPrintParseError . fmap calculation1 . parseCard

calculation1 :: [Card] -> Int
calculation1 = sum . map calculatePoints1 . findWinningNumbersEverywhere

calculatePoints1 :: [Int] -> Int
calculatePoints1 = foldr (\_ accum -> if accum == 0 then 1 else accum * 2) 0

findWinningNumbersEverywhere :: [Card] -> [[Int]]
findWinningNumbersEverywhere = map findWinningNumbers

findWinningNumbers :: Card -> [Int]
findWinningNumbers card = Set.toList $ Set.intersection targetNumbersSet gotNumbersSet
  where
    targetNumbersSet = Set.fromList card.targetNumbers
    gotNumbersSet = Set.fromList card.gotNumbers

parseCard :: Text -> Either (ParseErrorBundle Text Void) [Card]
parseCard = parse parser filePath

type Parser = Parsec Void Text

parser :: Parser [Card]
parser = cardParser `sepEndBy` newline

cardParser :: Parser Card
cardParser = do
    id' <- cardIdParser
    void (char ':')
    void hspace
    -- TODO: refactor
    targetNumbers' <- someTill (do a <- decimal; hspace; return a) (char '|')
    void hspace
    gotNumbers' <- numbersParser
    return Card{cardId = id', targetNumbers = targetNumbers', gotNumbers = gotNumbers'}

cardIdParser :: Parser Int
cardIdParser = do
    void (chunk "Card")
    void hspace
    decimal

numbersParser :: Parser [Int]
numbersParser = decimal `sepBy1` hspace
