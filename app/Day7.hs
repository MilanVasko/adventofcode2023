module Day7 where

import Data.Foldable (maximum)
import Data.Map.Strict qualified as Map
import Text.Megaparsec (Parsec, choice, parse, sepEndBy1)
import Text.Megaparsec.Char (char, hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Util qualified as U

filePath :: FilePath
filePath = "data/day7.txt"

data HandBid = HandBid
    { hand :: [Card]
    , bid :: Int
    }
    deriving stock (Show)

data Card
    = Card2
    | Card3
    | Card4
    | Card5
    | Card6
    | Card7
    | Card8
    | Card9
    | CardT
    | CardJ
    | CardQ
    | CardK
    | CardA
    deriving stock (Show, Ord, Eq)

data HandType
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind
    deriving stock (Show, Ord, Eq)

data SortableHandElement
    = ElementCard Card
    | ElementHandType HandType
    deriving stock (Show, Ord, Eq)

run :: IO ()
run = U.loadAndRun' filePath calculate calculate

calculate :: Text -> Text
calculate =
    U.prettyPrintParseError
        . fmap (sum . zipWith (*) [1 ..] . map bid . sortOn createSortableHand)
        . parseHandBids

createSortableHand :: HandBid -> [SortableHandElement]
createSortableHand = createSortableHandFromCards . hand

createSortableHandFromCards :: [Card] -> [SortableHandElement]
createSortableHandFromCards cards = ElementHandType (findHandType cards) : map ElementCard cards

findHandType :: [Card] -> HandType
findHandType = findHandType' Map.empty
  where
    findHandType' :: Map Card Int -> [Card] -> HandType
    findHandType' counts [] = case Map.size counts of
        1 -> FiveOfAKind
        2 -> case maxValue of
            4 -> FourOfAKind
            3 -> FullHouse
            _ -> error "Invalid count"
        3 -> case maxValue of
            3 -> ThreeOfAKind
            2 -> TwoPair
            _ -> error "Invalid count"
        4 -> case maxValue of
            2 -> OnePair
            _ -> error "Invalid count"
        5 -> HighCard
        _ -> error "Invalid count of unique cards"
      where
        maxValue = maximum $ Map.elems counts
    findHandType' counts (c : cs) = findHandType' (Map.insertWith (+) c 1 counts) cs

parseHandBids :: Text -> Either (ParseErrorBundle Text Void) [HandBid]
parseHandBids = parse parser filePath

type Parser = Parsec Void Text

parser :: Parser [HandBid]
parser = parseHandBid `sepEndBy1` newline

parseHandBid :: Parser HandBid
parseHandBid = do
    hand' <- many parseCard
    void hspace
    bid' <- decimal
    return HandBid{hand = hand', bid = bid'}

parseCard :: Parser Card
parseCard =
    choice
        [ Card2 <$ char '2'
        , Card3 <$ char '3'
        , Card4 <$ char '4'
        , Card5 <$ char '5'
        , Card6 <$ char '6'
        , Card7 <$ char '7'
        , Card8 <$ char '8'
        , Card9 <$ char '9'
        , CardT <$ char 'T'
        , CardJ <$ char 'J'
        , CardQ <$ char 'Q'
        , CardK <$ char 'K'
        , CardA <$ char 'A'
        ]
