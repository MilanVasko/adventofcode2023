module Day9 where

import Data.Maybe (fromJust)
import Text.Megaparsec (Parsec, parse, sepBy1, sepEndBy)
import Text.Megaparsec.Char (hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (ParseErrorBundle)
import Util qualified as U

filePath :: FilePath
filePath = "data/day9.txt"

run :: IO ()
run = U.loadAndRun' filePath (calculate calculation1) (calculate $ const ())

calculate :: (Show a) => ([NonEmpty Int] -> a) -> Text -> Text
calculate fn = U.prettyPrintParseError . fmap fn . parseFile

calculation1 :: [NonEmpty Int] -> Int
calculation1 = sum . map (calculateNext . getLastElements . computeAllDifferenceLists)

calculateNext :: [Int] -> Int
calculateNext = foldl' (+) 0 . drop 1 . reverse

getLastElements :: [NonEmpty Int] -> [Int]
getLastElements = map last

computeAllDifferenceLists :: NonEmpty Int -> [NonEmpty Int]
computeAllDifferenceLists list = go [list] list
  where
    go accum list'
        | all (== 0) list' = reverse accum
        | otherwise = case nonEmpty (computeDifferenceList list') of
            Just newList -> go (newList : accum) newList
            Nothing -> reverse accum

computeDifferenceList :: NonEmpty Int -> [Int]
computeDifferenceList = go []
  where
    go accum (_ :| []) = reverse accum
    go accum (a :| b : rest) = go (b - a : accum) (b :| rest)

parseFile :: Text -> Either (ParseErrorBundle Text Void) [NonEmpty Int]
parseFile = parse parser filePath

type Parser = Parsec Void Text

parser :: Parser [NonEmpty Int]
-- TODO: figure out a way to do this without fromJust?
parser = fmap (fromJust . nonEmpty) (signed hspace decimal `sepBy1` hspace) `sepEndBy` newline
