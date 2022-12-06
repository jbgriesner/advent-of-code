module Day3 (solve_day3) where

import Data.Char
import Data.List

inputPath :: String
inputPath = "./data/input_day3"

input :: IO String
input = readFile inputPath

halfSplit :: String -> [String]
halfSplit l =
    let lgth = length l
    in internSplit (lgth `div` 2) l
    where internSplit n x = [(take n x), (drop n x)]

toInt :: Char -> Int
toInt c
     | ord c > 96 = (ord c) - 96
     | otherwise = (ord c) - 38

intersec xs ys = xs \\ (xs \\ ys)

toInts :: String -> [Int]
toInts s =
    let ints = map (map toInt) . halfSplit $ s
     in intersec (ints !! 0) (ints !! 1)

part1 :: String -> [[Int]]
part1 x = map toInts $ lines x

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

solve :: [[Int]] -> Int
solve l =  solving l 0
    where
        solving [] n = n
        solving (x:xs) n = solving xs (n + sum (rmdups x))

solve_day3 :: IO ()
solve_day3 = do
    putStr "     part1:"
    input >>= print . solve . part1
--    putStr "     part2:"
 --   input >>= print . solve . part2
