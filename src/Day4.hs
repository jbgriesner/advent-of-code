{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day4 (solve_day4) where

import Data.List
import Data.List.Split
import Utils (divide)

dayNum :: Int
dayNum = 4

inputPath :: String
inputPath = "./data/input_day4"

input :: IO String
input = readFile inputPath

splitRanges :: String -> Bool
splitRanges s =
    let
        [s1, s2] = splitOn "," s
        [s11, s12] = map (read :: String -> Int) $ splitOn "-" s1
        [s21, s22] = map (read :: String -> Int) $ splitOn "-" s2
    in (s11>=s21 && s11<=s22 && s12<=s22 && s12>=s21) || (s21>=s11 && s21<=s12 && s22<=s12 && s22>=s11)

part1 :: String -> [Bool]
part1 s = map splitRanges $ lines s

solve_part1 :: [Bool] -> Int
solve_part1 bs = sum $ map fromEnum bs

part2 :: String -> [Bool]
part2 s = map splitRangesSoft $ lines s

intersec :: Eq a => [a] -> [a] -> [a]
intersec xs ys = xs \\ (xs \\ ys)

splitRangesSoft :: String -> Bool
splitRangesSoft s =
    let
        [s1, s2] = splitOn "," s
        [s11, s12] = map (read :: String -> Int) $ splitOn "-" s1
        [s21, s22] = map (read :: String -> Int) $ splitOn "-" s2
        l1 = [s11..s12]
        l2 = [s21..s22]
    in not $ null $ intersec l1 l2

solve_part2 :: [Bool] -> Int
solve_part2 bs = sum $ map fromEnum bs

solve_day4 :: IO ()
solve_day4 =
    divide dayNum >> do
    putStr "     part1: "
    input >>= print . solve_part1 . part1
    putStr "     part2: "
    input >>= print . solve_part2 . part2
