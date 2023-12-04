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
run = U.loadAndRun' filePath (calculate calculation1) (calculate calculation2)

calculate :: (Show a) => ([Card] -> a) -> Text -> Text
calculate fn = U.prettyPrintParseError . fmap fn . parseCard

calculation1 :: [Card] -> Int
calculation1 = sum . map calculatePoints1 . findWinningNumbersEverywhere

calculation2 :: [Card] -> Int
calculation2 = countAllTheCards . map length . findWinningNumbersEverywhere

countAllTheCards :: [Int] -> Int
countAllTheCards matchingNumbers = countAllTheCards' 0 (take (length matchingNumbers) $ repeat 1) matchingNumbers

countAllTheCards' :: Int -> [Int] -> [Int] -> Int
countAllTheCards' accum [] [] = accum
countAllTheCards' accum (copy : copies) (matchingNumber : matchingNumbers) =
    countAllTheCards' (accum + copy) (addUpCopies copy matchingNumber copies) matchingNumbers
countAllTheCards' _ _ _ = error "Should not happen"

addUpCopies :: Int -> Int -> [Int] -> [Int]
addUpCopies step amount copies = addUpCopies' step [] amount copies
  where
    addUpCopies' :: Int -> [Int] -> Int -> [Int] -> [Int]
    addUpCopies' _ accum 0 copies' = reverse accum ++ copies'
    addUpCopies' _ accum _ [] = reverse accum
    addUpCopies' step' accum amount' (c : cs) = addUpCopies' step' ((c + step') : accum) (amount' - 1) cs

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
