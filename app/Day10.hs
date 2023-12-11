module Day10 where

import Data.Set qualified as Set
import Data.Text qualified as T
import Grid (Coords, Grid, (!))
import Grid qualified as G
import Text.Megaparsec (Parsec, choice, parse, sepEndBy1)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Error (ParseErrorBundle)
import Util qualified as U

-- TODO: get rid of all the error functions

data Pipe
    = PipeNorthSouth
    | PipeEastWest
    | PipeNorthEast
    | PipeNorthWest
    | PipeSouthWest
    | PipeSouthEast
    deriving stock (Eq, Show)

data Tile = Ground | StartingPosition | Pipe Pipe
    deriving stock (Eq, Show)

filePath :: FilePath
filePath = "data/day10.txt"

run :: IO ()
run = U.loadAndRun' filePath calculate calculate

calculate :: Text -> Text
calculate = U.prettyPrintParseError . fmap calculation1 . parseFile

calculation1 :: Grid Tile -> Maybe Int
calculation1 g = fmap steps $ walkPipes g <$> G.findCoords (== StartingPosition) g

data WalkingState = WalkingState
    { currentCoords :: [Coords]
    , previousCoords :: [Coords]
    , steps :: Int
    }
    deriving stock (Show)

walkPipes :: Grid Tile -> Coords -> WalkingState
walkPipes g startingCoords =
    go
        WalkingState
            { currentCoords = currentCoords'
            , previousCoords = map (const startingCoords) currentCoords'
            , steps = 1
            }
  where
    currentCoords' = findWaysToGo g startingCoords

    go :: WalkingState -> WalkingState
    go ws = case Set.toList $ Set.fromList ws.currentCoords of
        [] -> error "The set of unique paths forward should not be empty"
        [_] -> ws
        _ ->
            go
                WalkingState
                    { currentCoords = zipWith (findWayToGo g) ws.previousCoords ws.currentCoords
                    , previousCoords = ws.currentCoords
                    , steps = ws.steps + 1
                    }

findWayToGo :: Grid Tile -> Coords -> Coords -> Coords
findWayToGo g src current = case filter (/= src) $ findWaysToGo g current of
    [x] -> x
    _ -> error "There should be exactly one way forward"

findWaysToGo :: Grid Tile -> Coords -> [Coords]
findWaysToGo g c =
    let neighbourCoords = G.neighbourCoordsHV g c
     in filter (canGo g c) neighbourCoords

canGo :: Grid Tile -> Coords -> Coords -> Bool
canGo g src@(sx, sy) dst@(dx, dy) =
    if dy == sy
        then
            if dx == sx
                then error "The coords should not all be equal"
                else
                    if dx < sx
                        then connectsEast dTile && connectsWest sTile
                        else connectsWest dTile && connectsEast sTile
        else
            if dx /= dx
                then error "The coords should not be diagonal"
                else
                    if dy < sy
                        then connectsSouth dTile && connectsNorth sTile
                        else connectsNorth dTile && connectsSouth sTile
  where
    sTile = g ! src
    dTile = g ! dst

connectsEast :: Tile -> Bool
connectsEast t = t == StartingPosition || t == Pipe PipeEastWest || t == Pipe PipeNorthEast || t == Pipe PipeSouthEast

connectsWest :: Tile -> Bool
connectsWest t = t == StartingPosition || t == Pipe PipeEastWest || t == Pipe PipeNorthWest || t == Pipe PipeSouthWest

connectsNorth :: Tile -> Bool
connectsNorth t = t == StartingPosition || t == Pipe PipeNorthSouth || t == Pipe PipeNorthEast || t == Pipe PipeNorthWest

connectsSouth :: Tile -> Bool
connectsSouth t = t == StartingPosition || t == Pipe PipeNorthSouth || t == Pipe PipeSouthWest || t == Pipe PipeSouthEast

parseFile :: Text -> Either (ParseErrorBundle Text Void) (Grid Tile)
parseFile = parse parser filePath

type Parser = Parsec Void Text

parser :: Parser (Grid Tile)
parser = do
    -- TODO: why do we need filter (not . null)?
    tiles <- filter (not . null) <$> many parseTile `sepEndBy1` newline
    let rowCount = length tiles
    let colCount = findColCount tiles
    return $ G.fromList rowCount colCount (join tiles)

-- TODO: find out how to do this without error function
findColCount :: [[a]] -> Int
findColCount tiles = case Set.toList setOfLengths of
    [] -> error "Expected at least 1 column"
    [x] -> x
    l -> error (T.pack ("Rows in the grid are not the same length" ++ show l))
  where
    setOfLengths = Set.fromList $ map length tiles

parseTile :: Parser Tile
parseTile =
    choice
        [ Pipe PipeNorthSouth <$ char '|'
        , Pipe PipeEastWest <$ char '-'
        , Pipe PipeNorthEast <$ char 'L'
        , Pipe PipeNorthWest <$ char 'J'
        , Pipe PipeSouthWest <$ char '7'
        , Pipe PipeSouthEast <$ char 'F'
        , Ground <$ char '.'
        , StartingPosition <$ char 'S'
        ]
