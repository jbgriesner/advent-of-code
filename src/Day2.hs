{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day2 (solve_day2) where

import Utils (divide)

dayNum :: Int
dayNum = 2

inputPath :: String
inputPath = "./data/input_day2"

data Choice = Rock | Paper | Scissor deriving (Eq, Show, Enum)

data Wanted = Win | Loss | Draw deriving (Eq, Show)

data Score = Score {
    win :: Int,
    loss :: Int,
    eq :: Int
}

scores :: Score
scores = Score {
    win = 6,
    loss = 0,
    eq = 3
}

instance Ord Choice where
    compare Rock Paper = LT
    compare Rock Scissor = GT
    compare Rock Rock = EQ
    compare Paper Paper = EQ
    compare Paper Scissor = LT
    compare Paper Rock = GT
    compare Scissor Paper = GT
    compare Scissor Scissor = EQ
    compare Scissor Rock = LT

fromString :: String -> Choice
fromString "A" = Rock
fromString "X" = Rock

fromString "B" = Paper
fromString "Y" = Paper

fromString "C" = Scissor
fromString "Z" = Scissor

toStr :: Choice -> String
toStr Rock = "X"
toStr Paper = "Y"
toStr Scissor = "Z"

nextChoice :: Wanted -> Choice -> Choice
nextChoice Win Rock = Paper
nextChoice Win Paper = Scissor
nextChoice Win Scissor = Rock
nextChoice Loss Rock = Scissor
nextChoice Loss Paper = Rock
nextChoice Loss Scissor = Paper
nextChoice Draw x = x

fromString_part2 :: String -> String
fromString_part2 a =
    let
        [x, y] = words a
        elf_choice = fromString x
        wanted_outcome = wanted y
        wanted_choice = toStr $ nextChoice wanted_outcome elf_choice
    in
        x ++ " " ++ wanted_choice
    where
        wanted "X" = Loss
        wanted "Y" = Draw
        wanted "Z" = Win

toInt :: Choice -> Int
toInt Rock = 1
toInt Paper = 2
toInt Scissor = 3

fight :: Choice -> Choice -> Int
fight x y
    | x == y = toInt y + eq scores
    | x > y = toInt y + loss scores
    | otherwise = toInt y + win scores

input :: IO String
input = readFile inputPath

part1 :: String -> [Int]
part1 x = map (\[x, y] -> fight x y) $ map (map fromString . words) $ lines x

solve :: [Int] -> Int
solve = sum

part2 :: String -> [Int]
part2 x = map (\[x, y] -> fight x y) $ map (map fromString . words) $ map fromString_part2 $ lines x

solve_day2 :: IO ()
solve_day2 =
    divide dayNum >> do
    putStr "     part1: "
    input >>= print . solve . part1
    putStr "     part2: "
    input >>= print . solve . part2
