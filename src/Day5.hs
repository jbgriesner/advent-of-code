module Day5 (solve_day5) where

import Data.List.Split
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

data Move = Move {
    from :: Int,
    to :: Int,
    count :: Int
} deriving (Show)

type Crates = HashMap Int [Char]
type Inputs = (Crates, [Move])

inputPath :: String
inputPath = "./data/input_day5"

input :: IO String
input = readFile inputPath

process :: String -> Char
process s
    | length s > 0 = head $ tail s
    | otherwise = ' '

stack :: [[Char]]
stack = []

toMaybeString :: String -> [Maybe Char]
toMaybeString [] = []
toMaybeString (' ' : ' ' : ' ' : ' ' : as) = Nothing : toMaybeString as
toMaybeString (a : ' ' : ' ' : ' ' : ' ' : as) = Just a : Nothing : toMaybeString as
toMaybeString (a : as) = Just a : toMaybeString as

check :: Int -> [Maybe Char] -> [Maybe Char]
check n l
    | length l == n = l
    | otherwise = l ++ f (n-length l)
        where
            f 0 = []
            f n = Nothing : f (n-1)

solve_1 :: Crates -> Move -> Crates
solve_1 crates (Move from to count) = 
    let 
        src = fromMaybe [] (HM.lookup from crates)
        dst = fromMaybe [] (HM.lookup to crates)
    in 
        HM.insert to (reverse (take count src) ++ dst) (HM.insert from (drop count src) crates)

solve_2 :: Crates -> Move -> Crates
solve_2 crates (Move from to count) = 
    let 
        src = fromMaybe [] (HM.lookup from crates)
        dst = fromMaybe [] (HM.lookup to crates)
    in 
        HM.insert to (take count src ++ dst) (HM.insert from (drop count src) crates)

part :: Int -> String -> Crates
part n s =
    let
        [stacks', moves''] = splitOn "\n\n" s
        stacks'' = lines stacks'
        moves' = lines moves''
        moves = map toMove moves'
        stacks_number = length $ filter (/= "") $ splitOn " " $ last $ stacks''
        (_:stacks''') = reverse stacks''
        stacks'''' = map (map process . splitOn " ") stacks'''
        stacks''''' = map toMaybeString stacks''''
        stacks'''''' = map (check stacks_number) stacks'''''
        stacks = foldl addLine HM.empty stacks''''''        
    in
        if n == 1
        then foldl solve_1 stacks moves
        else foldl solve_2 stacks moves
    where 
        toMove :: String -> Move
        toMove s =
            let 
                t = splitOn " " s
            in Move (read (t!!3)) (read (t!!5)) (read (t!!1))

        addLine :: Crates -> [Maybe Char] -> Crates
        addLine crates line = foldl addCrate crates (zip [1, 2..] line)

        addCrate :: Crates -> (Int, Maybe Char) -> Crates
        addCrate crate (_, Nothing) = crate
        addCrate crate (n, Just c) = 
            let crateN = fromMaybe [] (HM.lookup n crate)
            in HM.insert n (c : crateN) crate

solvePart :: Crates -> String
solvePart c = 
    let l = HM.toList c
    in map (head . snd) l

solve_day5 :: IO ()
solve_day5 = do
    putStr "     part1: "
    input >>= print . solvePart . part 1
    putStr "     part2: "
    input >>= print . solvePart . part 2
