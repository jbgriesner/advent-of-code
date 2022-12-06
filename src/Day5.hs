module Day5 (solve_day5) where

import Data.Char
import Data.List
import Control.Monad
import Data.List.Split

inputPath :: String
inputPath = "./data/input_day5"

input :: IO String
input = readFile inputPath

part1 :: String -> [Bool]
part1 s = map splitRanges $ lines s

solve_part1 :: [Bool] -> Int
solve_part1 bs = sum $ map fromEnum bs

part2 :: String -> [Bool]
part2 s = map splitRangesSoft $ lines s

solve_part2 :: [Bool] -> Int
solve_part2 bs = sum $ map fromEnum bs

solve_day5 :: IO ()
solve_day5 = do
    putStr "     part1: "
    input >>= print . solve_part1 . part1
    putStr "     part2: "
    input >>= print . solve_part2 . part2
