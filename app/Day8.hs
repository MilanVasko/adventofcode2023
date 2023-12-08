module Day8 where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Text.Megaparsec (Parsec, choice, parse, sepEndBy1)
import Text.Megaparsec.Char (char, digitChar, hspace, newline, upperChar)
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
run = U.loadAndRun' filePath (calculate calculation1) (calculate calculation2)

calculate :: (Show a) => (Journey -> a) -> Text -> Text
calculate fn = U.prettyPrintParseError . fmap (fn . journeyFromDefinition) . parseFile

calculation1 :: Journey -> Int
calculation1 = performHumanSteps

calculation2 :: Journey -> Int
calculation2 = performGhostSteps

parseFile :: Text -> Either (ParseErrorBundle Text Void) ([Instruction], [NodeDefinition])
parseFile = parse parser filePath

performHumanSteps :: Journey -> Int
performHumanSteps j = performHumanSteps' (Node "AAA") 0 (cycle j.instructions)
  where
    performHumanSteps' :: Node -> Int -> [Instruction] -> Int
    performHumanSteps' (Node "ZZZ") accum _ = accum
    performHumanSteps' _ _ [] = error "Instructions can not be empty"
    performHumanSteps' n accum (i : is) = performHumanSteps' (applyInstruction j.nodeMap i n) (accum + 1) is

performGhostSteps :: Journey -> Int
performGhostSteps j = foldr lcm 1 individualStepsUntilEnd
  where
    startingNodes = findStartingNodes j.nodeMap
    individualStepsUntilEnd = performStepsUntilEndingNode j.nodeMap (cycle j.instructions) <$> startingNodes

performGhostSteps' :: Map Node (Node, Node) -> [Node] -> Int -> [Instruction] -> Int
performGhostSteps' _ _ _ [] = error "Instructions can not be empty"
performGhostSteps' m nodes accum (i : is) =
    if all isEndingNode nodes
        then accum
        else performGhostSteps' m (map (applyInstruction m i) nodes) (accum + 1) is

performStepsUntilEndingNode :: Map Node (Node, Node) -> [Instruction] -> Node -> Int
performStepsUntilEndingNode m = performStepsUntilEndingNode' m 0

performStepsUntilEndingNode' :: Map Node (Node, Node) -> Int -> [Instruction] -> Node -> Int
performStepsUntilEndingNode' _ _ [] _ = error "Instructions can not be empty"
performStepsUntilEndingNode' m accum (i : is) n =
    if isEndingNode n
        then accum
        else performStepsUntilEndingNode' m (accum + 1) is (applyInstruction m i n)

findStartingNodes :: Map Node (Node, Node) -> [Node]
findStartingNodes = filter isStartingNode . Map.keys

isStartingNode :: Node -> Bool
isStartingNode (Node n) = T.isSuffixOf "A" n

isEndingNode :: Node -> Bool
isEndingNode (Node n) = T.isSuffixOf "Z" n

applyInstruction :: Map Node (Node, Node) -> Instruction -> Node -> Node
applyInstruction m i node = extract i . fromJust $ Map.lookup node m
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
    name <- parseNodeName
    void hspace
    void (char '=')
    void hspace
    void (char '(')
    other1 <- parseNodeName
    void (char ',')
    void hspace
    other2 <- parseNodeName
    void (char ')')
    return $ NodeDefinition{name = T.pack name, other = (T.pack other1, T.pack other2)}

parseNodeName :: Parser String
parseNodeName = many $ choice [upperChar, digitChar]
