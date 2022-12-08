
module Day9 (solve_day9) where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Utils (divide)

dayNum :: Int
dayNum = 9

inputPath :: String
inputPath = "./data/input_day9"

input :: IO String
input = readFile inputPath

part :: Int -> String -> Int
part p s
  | p == 1 = 1
  | otherwise = 2

solve_day8 :: IO ()
solve_day8 =
    divide dayNum >> do
    putStr "     part1: "
    input >>= print . part 1
    putStr "     part2: "
    input >>= print . part 2
