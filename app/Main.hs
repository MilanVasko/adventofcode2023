module Main where

import Data.Text (append)
import Day1 qualified
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $ runDay 1 Day1.run

runDay :: Int -> IO () -> IO ()
runDay n f = putText ("Day " `append` show n `append` ": ") >> f
