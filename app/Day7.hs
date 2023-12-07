module Day7 where

import Data.Foldable (maximum)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Text.Megaparsec (Parsec, choice, parse, sepEndBy1)
import Text.Megaparsec.Char (char, hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Util qualified as U

-- TODO: refactor

filePath :: FilePath
filePath = "data/day7.txt"

data HandBid = HandBid
    { hand :: [Card]
    , bid :: Int
    }
    deriving stock (Show)

data Card
    = CardSpecialJ
    | Card2
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
run = U.loadAndRun' filePath calculate1 calculate2

calculate1 :: Text -> Text
calculate1 =
    U.prettyPrintParseError
        . fmap (sum . zipWith (*) [1 ..] . map bid . sortOn createOrdinarySortableHand)
        . parseHandBids False

calculate2 :: Text -> Text
calculate2 =
    U.prettyPrintParseError
        . fmap (sum . zipWith (*) [1 ..] . map bid . sortOn createSpecialSortableHand)
        . parseHandBids True

createOrdinarySortableHand :: HandBid -> [SortableHandElement]
createOrdinarySortableHand = createOrdinarySortableHandFromCards . hand

createOrdinarySortableHandFromCards :: [Card] -> [SortableHandElement]
createOrdinarySortableHandFromCards cards = ElementHandType (findOrdinaryHandType cards) : map ElementCard cards

findOrdinaryHandType :: [Card] -> HandType
findOrdinaryHandType = findOrdinaryHandType' Map.empty
  where
    findOrdinaryHandType' :: Map Card Int -> [Card] -> HandType
    findOrdinaryHandType' counts [] = case Map.size counts of
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
    findOrdinaryHandType' counts (c : cs) = findOrdinaryHandType' (Map.insertWith (+) c 1 counts) cs

createSpecialSortableHand :: HandBid -> [SortableHandElement]
createSpecialSortableHand = createSpecialSortableHandFromCards . hand

createSpecialSortableHandFromCards :: [Card] -> [SortableHandElement]
createSpecialSortableHandFromCards cards = ElementHandType (findSpecialHandType cards) : map ElementCard cards

findSpecialHandType :: [Card] -> HandType
findSpecialHandType = findSpecialHandType' Map.empty
  where
    findSpecialHandType' :: Map Card Int -> [Card] -> HandType
    findSpecialHandType' counts [] = case Map.size countsWithoutJ of
        0 -> FiveOfAKind
        1 -> FiveOfAKind
        2 -> case maxValue of
            4 -> FourOfAKind
            3 -> FullHouse
            _ -> error $ "Invalid count: " `T.append` show maxValue
        3 -> case maxValue of
            3 -> ThreeOfAKind
            2 -> TwoPair
            _ -> error $ "Invalid count: " `T.append` show maxValue
        4 -> case maxValue of
            2 -> OnePair
            _ -> error $ "Invalid count: " `T.append` show maxValue
        5 -> HighCard
        _ -> error "Invalid count of unique cards"
      where
        countsWithoutJ = Map.filterWithKey (\k _ -> k /= CardSpecialJ) counts
        jCount = fromMaybe 0 $ Map.lookup CardSpecialJ counts
        maxValue = maximum (Map.elems countsWithoutJ) + jCount
    findSpecialHandType' counts (c : cs) = findSpecialHandType' (Map.insertWith (+) c 1 counts) cs

parseHandBids :: Bool -> Text -> Either (ParseErrorBundle Text Void) [HandBid]
parseHandBids treatJSpecially = parse (parser treatJSpecially) filePath

type Parser = Parsec Void Text

parser :: Bool -> Parser [HandBid]
parser treatJSpecially = parseHandBid treatJSpecially `sepEndBy1` newline

parseHandBid :: Bool -> Parser HandBid
parseHandBid treatJSpecially = do
    hand' <- many (parseCard treatJSpecially)
    void hspace
    bid' <- decimal
    return HandBid{hand = hand', bid = bid'}

parseCard :: Bool -> Parser Card
parseCard treatJSpecially =
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
        , (if treatJSpecially then CardSpecialJ else CardJ) <$ char 'J'
        , CardQ <$ char 'Q'
        , CardK <$ char 'K'
        , CardA <$ char 'A'
        ]
