module Util where

import Data.Text qualified as T
import Data.Text.Read qualified as T
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Error (ParseErrorBundle)

type ComputeFunction a = Text -> a

loadAndRun :: (Show a) => (Show b) => FilePath -> ComputeFunction a -> ComputeFunction b -> IO ()
loadAndRun path f1 f2 = loadAndRun' path (show . f1) (show . f2)

loadAndRun' :: FilePath -> ComputeFunction Text -> ComputeFunction Text -> IO ()
loadAndRun' path f1 f2 = do
    content <- decodeUtf8 <$> readFileBS path
    putTextLn $ f1 content `T.append` ", " `T.append` f2 content

textToInt :: Text -> Maybe Int
textToInt t = case T.decimal t of
    Right (n, _) -> Just n
    Left _ -> Nothing

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple fn (a, b) = (fn a, fn b)

liftMaybeFromTuple :: (Maybe a, Maybe b) -> Maybe (a, b)
liftMaybeFromTuple (Just a, Just b) = Just (a, b)
liftMaybeFromTuple (_, _) = Nothing

concatInts :: (Int, Int) -> Int
concatInts (a, b) = 10 * a + b

prettyPrintParseError :: (Show s) => Either (ParseErrorBundle Text Void) s -> Text
prettyPrintParseError (Right x) = show x
prettyPrintParseError (Left x) = T.pack $ errorBundlePretty x
