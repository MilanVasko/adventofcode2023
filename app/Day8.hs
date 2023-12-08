module Day8 where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Text.Megaparsec (Parsec, choice, parse, sepEndBy1)
import Text.Megaparsec.Char (char, hspace, newline, upperChar)
import Text.Megaparsec.Error (ParseErrorBundle)
import Util qualified as U

filePath :: FilePath
filePath = "data/day8.txt"

data Instruction
    = GoLeft
    | GoRight
    deriving stock (Show)

newtype Node = Node Text
    deriving newtype (Eq, Ord, Show)

data NodeDefinition = NodeDefinition
    { name :: Text
    , other :: (Text, Text)
    }
    deriving stock (Show)

data Journey = Journey
    { instructions :: [Instruction]
    , nodeMap :: Map Node (Node, Node)
    }
    deriving stock (Show)

run :: IO ()
run = U.loadAndRun' filePath calculate calculate

calculate :: Text -> Text
calculate = U.prettyPrintParseError . fmap (performSteps . journeyFromDefinition) . parseFile

parseFile :: Text -> Either (ParseErrorBundle Text Void) ([Instruction], [NodeDefinition])
parseFile = parse parser filePath

performSteps :: Journey -> Int
performSteps j = performSteps' (Node "AAA") 0 (cycle j.instructions)
  where
    performSteps' :: Node -> Int -> [Instruction] -> Int
    performSteps' (Node "ZZZ") accum _ = accum
    performSteps' _ _ [] = error "Instructions can not be empty"
    performSteps' currentNode accum (i : is) = performSteps' (applyInstruction i currentNode j.nodeMap) (accum + 1) is

applyInstruction :: Instruction -> Node -> Map Node (Node, Node) -> Node
applyInstruction i node m = extract i . fromJust $ Map.lookup node m
  where
    extract :: Instruction -> ((a, a) -> a)
    extract GoLeft = fst
    extract GoRight = snd

journeyFromDefinition :: ([Instruction], [NodeDefinition]) -> Journey
journeyFromDefinition (instructions, nodeDefinitions) =
    Journey
        { instructions = instructions
        , nodeMap = createNodeMap Map.empty nodeDefinitions
        }
  where
    createNodeMap :: Map Node (Node, Node) -> [NodeDefinition] -> Map Node (Node, Node)
    createNodeMap accum [] = accum
    createNodeMap accum (n : ns) = createNodeMap (Map.insert (Node n.name) (bimap Node Node n.other) accum) ns

type Parser = Parsec Void Text

parser :: Parser ([Instruction], [NodeDefinition])
parser = do
    instructions <- many parseInstruction
    void newline
    void newline
    nodes <- parseNode `sepEndBy1` newline
    return (instructions, nodes)

parseInstruction :: Parser Instruction
parseInstruction = choice [GoLeft <$ char 'L', GoRight <$ char 'R']

parseNode :: Parser NodeDefinition
parseNode = do
    name <- many upperChar
    void hspace
    void (char '=')
    void hspace
    void (char '(')
    other1 <- many upperChar
    void (char ',')
    void hspace
    other2 <- many upperChar
    void (char ')')
    return $ NodeDefinition{name = T.pack name, other = (T.pack other1, T.pack other2)}
