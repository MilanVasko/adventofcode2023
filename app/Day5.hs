module Day5 where

-- TODO: REFACTOR EVERYTHING!!!

import Data.Set qualified as Set
import Text.Megaparsec (Parsec, chunk, parse, sepBy1, sepEndBy, someTill)
import Text.Megaparsec.Char (char, hspace, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Util qualified as U

filePath :: FilePath
filePath = "data/day5.txt"

data MappingDefinition s d = MappingDefinition
    { destinationStart :: d
    , sourceStart :: s
    , rangeLength :: Int
    }
    deriving stock (Show)

newtype SeedIndex = SeedIndex Int
    deriving newtype (Eq, Num, Ord, Show)
newtype FertilizerIndex = FertilizerIndex Int
    deriving newtype (Eq, Num, Ord, Show)
newtype HumidityIndex = HumidityIndex Int
    deriving newtype (Eq, Num, Ord, Show)
newtype LightIndex = LightIndex Int
    deriving newtype (Eq, Num, Ord, Show)
newtype LocationIndex = LocationIndex Int
    deriving newtype (Eq, Num, Ord, Show)
newtype SoilIndex = SoilIndex Int
    deriving newtype (Eq, Num, Ord, Show)
newtype TemperatureIndex = TemperatureIndex Int
    deriving newtype (Eq, Num, Ord, Show)
newtype WaterIndex = WaterIndex Int
    deriving newtype (Eq, Num, Ord, Show)

data Almanac = Almanac
    { seeds :: [SeedIndex]
    , seedToSoilMap :: [MappingDefinition SeedIndex SoilIndex]
    , soilToFertilizerMap :: [MappingDefinition SoilIndex FertilizerIndex]
    , fertilizerToWaterMap :: [MappingDefinition FertilizerIndex WaterIndex]
    , waterToLightMap :: [MappingDefinition WaterIndex LightIndex]
    , lightToTemperatureMap :: [MappingDefinition LightIndex TemperatureIndex]
    , temperatureToHumidityMap :: [MappingDefinition TemperatureIndex HumidityIndex]
    , humidityToLocationMap :: [MappingDefinition HumidityIndex LocationIndex]
    }
    deriving stock (Show)

run :: IO ()
run = U.loadAndRun' filePath (calculate calculation2) (calculate calculation2)

calculate :: (Almanac -> Int) -> Text -> Text
calculate fn = U.prettyPrintParseError . fmap fn . parseAlmanac

calculation1 :: Almanac -> Int
calculation1 almanac =
    foldr (\(LocationIndex item) accum -> min item accum) maxInt $
        map (\seed' -> calculateIndex HumidityIndex (\(HumidityIndex ss) -> LocationIndex ss) seed' almanac.humidityToLocationMap) $
            map (\seed' -> calculateIndex TemperatureIndex (\(TemperatureIndex ss) -> HumidityIndex ss) seed' almanac.temperatureToHumidityMap) $
                map (\seed' -> calculateIndex LightIndex (\(LightIndex ss) -> TemperatureIndex ss) seed' almanac.lightToTemperatureMap) $
                    map (\seed' -> calculateIndex WaterIndex (\(WaterIndex ss) -> LightIndex ss) seed' almanac.waterToLightMap) $
                        map (\seed' -> calculateIndex FertilizerIndex (\(FertilizerIndex ss) -> WaterIndex ss) seed' almanac.fertilizerToWaterMap) $
                            map (\seed' -> calculateIndex SoilIndex (\(SoilIndex ss) -> FertilizerIndex ss) seed' almanac.soilToFertilizerMap) $
                                map (\seed' -> calculateIndex SeedIndex (\(SeedIndex ss) -> SoilIndex ss) seed' almanac.seedToSoilMap) $
                                    almanac.seeds

calculation2 :: Almanac -> Int
calculation2 almanac =
    foldr (\(LocationIndex item) accum -> min item accum) maxInt $
        map (\seed' -> calculateIndex HumidityIndex (\(HumidityIndex ss) -> LocationIndex ss) seed' almanac.humidityToLocationMap) $
            map (\seed' -> calculateIndex TemperatureIndex (\(TemperatureIndex ss) -> HumidityIndex ss) seed' almanac.temperatureToHumidityMap) $
                map (\seed' -> calculateIndex LightIndex (\(LightIndex ss) -> TemperatureIndex ss) seed' almanac.lightToTemperatureMap) $
                    map (\seed' -> calculateIndex WaterIndex (\(WaterIndex ss) -> LightIndex ss) seed' almanac.waterToLightMap) $
                        map (\seed' -> calculateIndex FertilizerIndex (\(FertilizerIndex ss) -> WaterIndex ss) seed' almanac.fertilizerToWaterMap) $
                            map (\seed' -> calculateIndex SoilIndex (\(SoilIndex ss) -> FertilizerIndex ss) seed' almanac.soilToFertilizerMap) $
                                map (\seed' -> calculateIndex SeedIndex (\(SeedIndex ss) -> SoilIndex ss) seed' almanac.seedToSoilMap) $
                                    (moreSeedsForCalculation2 almanac.seeds)

moreSeedsForCalculation2 :: [SeedIndex] -> [SeedIndex]
moreSeedsForCalculation2 = moreSeedsForCalculation2' []
  where
    moreSeedsForCalculation2' :: [SeedIndex] -> [SeedIndex] -> [SeedIndex]
    moreSeedsForCalculation2' accum [] = accum
    moreSeedsForCalculation2' _ [_] = error "TODO"
    moreSeedsForCalculation2' accum (SeedIndex a : SeedIndex b : rest) = moreSeedsForCalculation2' (map SeedIndex [a .. a + b - 1] ++ accum) rest

calculateIndex :: (Ord s, Num s, Num d) => (Int -> s) -> (s -> d) -> s -> [MappingDefinition s d] -> d
calculateIndex sc sd source definitions = case filter (\d -> source >= d.sourceStart && source < d.sourceStart + sc d.rangeLength) definitions of
    (x : _) -> sd source + (x.destinationStart - sd x.sourceStart)
    _ -> sd source

parseAlmanac :: Text -> Either (ParseErrorBundle Text Void) Almanac
parseAlmanac = parse parser filePath

type Parser = Parsec Void Text

parser :: Parser Almanac
parser = do
    seeds' <- seedsParser
    void $ many newline
    seedToSoilMap' <- mapParser SeedIndex SoilIndex "seed-to-soil"
    void $ many newline
    soilToFertilizerMap' <- mapParser SoilIndex FertilizerIndex "soil-to-fertilizer"
    void $ many newline
    fertilizerToWater' <- mapParser FertilizerIndex WaterIndex "fertilizer-to-water"
    void $ many newline
    waterToLight' <- mapParser WaterIndex LightIndex "water-to-light"
    void $ many newline
    lightToTemperature' <- mapParser LightIndex TemperatureIndex "light-to-temperature"
    void $ many newline
    temperatureToHumidity' <- mapParser TemperatureIndex HumidityIndex "temperature-to-humidity"
    void $ many newline
    humidityToLocation' <- mapParser HumidityIndex LocationIndex "humidity-to-location"

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

seedsParser :: Parser [SeedIndex]
seedsParser = do
    void (chunk "seeds")
    void (char ':')
    void hspace
    map SeedIndex <$> decimal `sepBy1` hspace

mapParser :: (Int -> s) -> (Int -> d) -> Text -> Parser [MappingDefinition s d]
mapParser source dest name = do
    void (chunk name)
    void hspace
    void (chunk "map")
    void (char ':')
    void newline
    many (sourceDestinationRangeParser source dest)

sourceDestinationRangeParser :: (Int -> s) -> (Int -> d) -> Parser (MappingDefinition s d)
sourceDestinationRangeParser source dest = do
    destinationRangeStart <- dest <$> decimal
    void hspace
    sourceRangeStart <- source <$> decimal
    void hspace
    rangeLength <- decimal
    void newline
    return $ MappingDefinition destinationRangeStart sourceRangeStart rangeLength
