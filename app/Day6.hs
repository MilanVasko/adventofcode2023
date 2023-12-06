module Day6 where

-- TODO: REFACTOR EVERYTHING!!!

import Text.Megaparsec (Parsec, chunk, parse, sepBy1)
import Text.Megaparsec.Char (char, hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Util qualified as U

filePath :: FilePath
filePath = "data/day6.txt"

run :: IO ()
run = U.loadAndRun' filePath calculate calculate

calculate :: Text -> Text
calculate = U.prettyPrintParseError . fmap calculation1 . parseRaces

calculation1 :: [Race] -> Int
calculation1 = product . map getRecordHoldings

getRecordHoldings :: Race -> Int
getRecordHoldings race = length $ filter (race.distance <) $ map (distanceTravelled race.time) [1 .. race.time - 1]

distanceTravelled :: Int -> Int -> Int
distanceTravelled timeAvailable msHolding = timeToTravel * msHolding
  where
    timeToTravel = timeAvailable - msHolding

data Race = Race
    { time :: Int
    , distance :: Int
    }
    deriving stock (Show)

parseRaces :: Text -> Either (ParseErrorBundle Text Void) [Race]
parseRaces = parse parser filePath

type Parser = Parsec Void Text

parser :: Parser [Race]
parser = do
    void (chunk "Time")
    void (char ':')
    void hspace
    times <- decimal `sepBy1` hspace
    void newline
    void (chunk "Distance")
    void (char ':')
    void hspace
    distances <- decimal `sepBy1` hspace
    return $ buildRaces times distances

buildRaces :: [Int] -> [Int] -> [Race]
buildRaces = buildRaces' []
  where
    buildRaces' :: [Race] -> [Int] -> [Int] -> [Race]
    buildRaces' accum [] [] = reverse accum
    buildRaces' accum (t : times) (d : distances) = buildRaces' (Race t d : accum) times distances
    buildRaces' _ _ _ = error "Shouldn't happen"
