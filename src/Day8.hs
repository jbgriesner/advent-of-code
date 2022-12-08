
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
--inputPath = "./data/test"

input :: IO String
input = readFile inputPath

data Matrix = Matrix {
    rows :: [[Int]]
} deriving (Show)

getRow :: Int -> Matrix -> [Int]
getRow n (Matrix m) = (m!!n)

getCol :: Int -> Matrix -> [Int]
getCol n (Matrix m) =
    let
        m' = transpose m
    in getRow n (Matrix m')

before :: Int -> [Int] -> [Int]
before _ [] = []
before j (x:xs)
  | j > 0 = x : before (j-1) xs
  | otherwise = []

after :: Int -> [Int] -> [Int]
after j l =
    let
        l' = before (length l - j - 1) $ reverse l
    in reverse l'

getUp :: Int -> Int -> Matrix -> [Int]
getUp i j m =
    let
        col = getCol i m
    in reverse $ before j col

getDown :: Int -> Int -> Matrix -> [Int]
getDown i j m =
    let
        col = getCol i m
    in after j col

getRight :: Int -> Int -> Matrix -> [Int]
getRight i j m =
    let
        row = getRow j m
    in after i row

getLeft :: Int -> Int -> Matrix -> [Int]
getLeft i j m =
    let
        row = getRow j m
    in reverse $ before i row

stringToInts :: String -> [Int]
stringToInts = map $ (read . (:""))

getElem :: Int -> Int -> Matrix -> Int
getElem x y m = (getRow y m)!!x

isVisible :: Int -> Int -> Matrix -> [Bool]
isVisible x y m =
    let
        r = getRight x y m
        l = getLeft x y m
        t = getUp x y m
        d = getDown x y m
        el = getElem x y m
     in [not $ any (>=el) s | s <- [r ,l, t, d]]

score :: Int -> [Int] -> Int
score _ [] = 0
score m (x:xs)
    | m > x = 1 + score m xs
    | otherwise = 1

scenicScore :: Int -> Int -> Matrix -> [Int]
scenicScore x y m =
    let
        r = getRight x y m
        l = getLeft x y m
        t = getUp x y m
        d = getDown x y m
        el = getElem x y m
     in [score el s | s <- [r ,l, t, d]]

part :: Int -> String -> Int
part p s
    | p == 1 =
        let
            l = lines s
            m = Matrix (map stringToInts l)
            i = [0..((length $ head l)-1)]
            j = [0..((length l)-1)]
            coords = [(x, y) | x <- i, y <- j]
            bools = map (\(x, y) -> isVisible x y m) coords
        in foldl (\c bs -> if any (==True) bs then c+1 else c) 0 bools
    | otherwise =
        let
            l = lines s
            m = Matrix (map stringToInts l)
            i = [0..((length $ head l)-1)]
            j = [0..((length l)-1)]
            coords = [(x, y) | x <- i, y <- j]
            scores = map (\(x, y) -> scenicScore x y m) coords
        in maxInList $ map (\[w, x, y, z] -> w*x*y*z) scores

maxInList :: [Int]->Int
maxInList [] = 0
maxInList l = foldr max 0 l

solve_day8 :: IO ()
solve_day8 =
    divide dayNum >> do
    putStr "     part1: "
    input >>= print . part 1
    putStr "     part2: "
    input >>= print . part 2
