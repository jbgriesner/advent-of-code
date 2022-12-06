
module Day1 (solve_day1) where

import Text.Read
import Data.List.Split
import Data.List

inputPath :: String
inputPath = "./data/input_day1"

input :: IO String
input = readFile inputPath

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

solve_day1 :: IO ()
solve_day1 = do
    putStr "     part2: "
    input >>= (print . solve) . process
