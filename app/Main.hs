module Main where

import Data.Text (append)
import Day1 qualified
import Day2 qualified
import Day3 qualified
import Day4 qualified
import Day6 qualified
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $ do
    runDay 1 Day1.run
    runDay 2 Day2.run
    runDay 3 Day3.run
    runDay 4 Day4.run
    -- runDay 5 Day5.run
    runDay 6 Day6.run

runDay :: Int -> IO () -> IO ()
runDay n f = putText ("Day " `append` show n `append` ": ") >> f
