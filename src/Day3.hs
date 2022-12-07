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
    where internSplit n x = [take n x, drop n x]

toInt :: Char -> Int
toInt c
     | ord c > 96 = ord c - 96
     | otherwise = ord c - 38

intersec xs ys = xs \\ (xs \\ ys)

inters xs ys zs = let
                        inner = intersec xs ys
                    in intersec inner zs

toInts :: String -> [Int]
toInts s =
    let ints = map (map toInt) . halfSplit $ s
     in intersec (ints !! 0) (ints !! 1)

part1 :: String -> [[Int]]
part1 x = map toInts $ lines x

toInts3 :: String -> [Int]
toInts3 s = map toInt s

groupBy3 :: [[a]] -> [[[a]]]
groupBy3 [] = []
groupBy3 (a:b:c:d) = [a, b, c] : (groupBy3 d)

part2 :: String -> [[Int]]
part2 x = let
            list = groupBy3 $ map toInts3 $ lines x
          in map (\[a, b, c] -> inters a b c) list

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

solve :: [[Int]] -> Int
solve l =  solving l 0
    where
        solving [] n = n
        solving (x:xs) n = solving xs (n + sum (rmdups x))

solve_part2 :: [[Int]] -> Int
solve_part2 l = sum $ concatMap rmdups l

solve_day3 :: IO ()
solve_day3 = do
    putStr "     part1: "
    input >>= print . solve . part1
    putStr "     part2: "
    input >>= print . solve_part2 . part2
