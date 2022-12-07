
module Day8 (solve_day8) where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Utils (divide)

dayNum :: Int
dayNum = 8

inputPath :: String
inputPath = "./data/input_day8"

input :: IO String
input = readFile inputPath

solve :: Int -> String -> Int
solve part s = 4

solve_day8 :: IO ()
solve_day8 =
    divide dayNum >> do
    putStr "     part1: "
    input >>= print . solve 1
    putStr "     part2: "
    input >>= print . solve 2
