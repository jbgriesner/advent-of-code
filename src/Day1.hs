
module Day1 (solve_day1) where

import Text.Read
import Data.List.Split
import Data.List hiding (sum)
import Utils (divide)
import Prelude hiding (sum)

dayNum :: Int
dayNum = 1

inputPath :: String
inputPath = "./data/input_day1"

input :: IO String
input = readFile inputPath

process1 :: String -> [Int]
process1 x = map sumAll $ splitWhen (==Nothing) $ map (readMaybe :: String -> Maybe Int) $ lines x

process :: String -> [Int]
process x = map sumAll $ splitWhen (==Nothing) $ map (readMaybe :: String -> Maybe Int) $ lines x

sumAll :: [Maybe Int] -> Int
sumAll [Just x] = x
sumAll (x:xs) = sumAll [x] + sumAll xs
sumAll _ = 0

solve :: [Int] -> Int
solve l = tup $ take 3 $ reverse $ sort l
    where tup [x, y, z] = x + y + z
          tup _ = 0

solve1 :: [Int] -> Int
solve1 l = head $ reverse $ sort l

solve_day1 :: IO ()
solve_day1 =
    divide dayNum >> do
    putStr "     part1: "
    input >>= (print . solve1) . process1
    putStr "     part2: "
    input >>= (print . solve) . process
