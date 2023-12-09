module Day5 where

-- TODO: refactor
-- TODO: optimize

import Text.Megaparsec (Parsec, chunk, parse, sepBy1)
import Text.Megaparsec.Char (char, hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Util qualified as U

filePath :: FilePath
filePath = "data/day5.txt"

data MappingDefinition = MappingDefinition
    { destinationStart :: Int
    , sourceStart :: Int
    , rangeLength :: Int
    }
    deriving stock (Show)

data Almanac = Almanac
    { seeds :: [Int]
    , seedToSoilMap :: [MappingDefinition]
    , soilToFertilizerMap :: [MappingDefinition]
    , fertilizerToWaterMap :: [MappingDefinition]
    , waterToLightMap :: [MappingDefinition]
    , lightToTemperatureMap :: [MappingDefinition]
    , temperatureToHumidityMap :: [MappingDefinition]
    , humidityToLocationMap :: [MappingDefinition]
    }
    deriving stock (Show)

run :: IO ()
run = U.loadAndRun' filePath (calculate calculation1) (const "TODO")

calculate :: (Almanac -> Int) -> Text -> Text
calculate fn = U.prettyPrintParseError . fmap fn . parseAlmanac

calculation1 :: Almanac -> Int
calculation1 almanac =
    foldr min maxInt $
        map (calculateIndex almanac.humidityToLocationMap) $
            map (calculateIndex almanac.temperatureToHumidityMap) $
                map (calculateIndex almanac.lightToTemperatureMap) $
                    map (calculateIndex almanac.waterToLightMap) $
                        map (calculateIndex almanac.fertilizerToWaterMap) $
                            map (calculateIndex almanac.soilToFertilizerMap) $
                                map (calculateIndex almanac.seedToSoilMap) $
                                    almanac.seeds

calculation2 :: Almanac -> Int
calculation2 almanac =
    foldr min maxInt $
        map (calculateIndex almanac.humidityToLocationMap) $
            map (calculateIndex almanac.temperatureToHumidityMap) $
                map (calculateIndex almanac.lightToTemperatureMap) $
                    map (calculateIndex almanac.waterToLightMap) $
                        map (calculateIndex almanac.fertilizerToWaterMap) $
                            map (calculateIndex almanac.soilToFertilizerMap) $
                                map (calculateIndex almanac.seedToSoilMap) $
                                    (moreSeedsForCalculation2 almanac.seeds)

moreSeedsForCalculation2 :: [Int] -> [Int]
moreSeedsForCalculation2 = moreSeedsForCalculation2' []
  where
    moreSeedsForCalculation2' :: [Int] -> [Int] -> [Int]
    moreSeedsForCalculation2' accum [] = accum
    moreSeedsForCalculation2' _ [_] = error "TODO"
    moreSeedsForCalculation2' accum (a : b : rest) = moreSeedsForCalculation2' ([a .. a + b - 1] ++ accum) rest

calculateIndex :: [MappingDefinition] -> Int -> Int
calculateIndex definitions source = case filter (\d -> source >= d.sourceStart && source < d.sourceStart + d.rangeLength) definitions of
    (x : _) -> source + (x.destinationStart - x.sourceStart)
    _ -> source

parseAlmanac :: Text -> Either (ParseErrorBundle Text Void) Almanac
parseAlmanac = parse parser filePath

type Parser = Parsec Void Text

parser :: Parser Almanac
parser = do
    seeds' <- seedsParser
    void $ many newline
    seedToSoilMap' <- mapParser "seed-to-soil"
    void $ many newline
    soilToFertilizerMap' <- mapParser "soil-to-fertilizer"
    void $ many newline
    fertilizerToWater' <- mapParser "fertilizer-to-water"
    void $ many newline
    waterToLight' <- mapParser "water-to-light"
    void $ many newline
    lightToTemperature' <- mapParser "light-to-temperature"
    void $ many newline
    temperatureToHumidity' <- mapParser "temperature-to-humidity"
    void $ many newline
    humidityToLocation' <- mapParser "humidity-to-location"

    return $
        Almanac
            { seeds = seeds'
            , seedToSoilMap = seedToSoilMap'
            , soilToFertilizerMap = soilToFertilizerMap'
            , fertilizerToWaterMap = fertilizerToWater'
            , waterToLightMap = waterToLight'
            , lightToTemperatureMap = lightToTemperature'
            , temperatureToHumidityMap = temperatureToHumidity'
            , humidityToLocationMap = humidityToLocation'
            }

seedsParser :: Parser [Int]
seedsParser = do
    void (chunk "seeds")
    void (char ':')
    void hspace
    decimal `sepBy1` hspace

mapParser :: Text -> Parser [MappingDefinition]
mapParser name = do
    void (chunk name)
    void hspace
    void (chunk "map")
    void (char ':')
    void newline
    many sourceDestinationRangeParser

sourceDestinationRangeParser :: Parser MappingDefinition
sourceDestinationRangeParser = do
    destinationRangeStart <- decimal
    void hspace
    sourceRangeStart <- decimal
    void hspace
    rangeLength <- decimal
    void newline
    return $ MappingDefinition destinationRangeStart sourceRangeStart rangeLength
