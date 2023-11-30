{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
module Day9 (solve_day9) where

import Data.List hiding (tails)
import Utils (divide, rmdups)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import Control.Monad
-- import qualified Data.Text (pack)
import Prelude hiding (Right, Down, Left, Up)
import qualified Data.Set as S

dayNum :: Int
dayNum = 9

input :: IO String
input = readFile $ "./data/input_day" <> show dayNum
-- input = readFile $ "./data/test"

data Motion = Right | Down | Left | Up deriving (Eq, Show)

fromString :: String -> [Motion]
fromString ('R':' ':x) = replicate (read x) Right
fromString ('U':' ':x) = replicate (read x) Up
fromString ('D':' ':x) = replicate (read x) Down
fromString ('L':' ':x) = replicate (read x) Left

type Coord = (Int, Int)

data Grid = Grid {
    head :: Coord,
    tails :: [Coord]
} deriving (Show)

data Grid10 = Grid10 {
    hs :: S.Set Coord,
    ts :: [Coord]
} deriving (Show)

initGrid :: Grid
initGrid = Grid (0, 0) [(0, 0)]

initGrid10 :: Grid10
initGrid10 = Grid10 S.empty $ replicate 10 (0, 0)

goRight, goLeft, goUp, goDown :: Coord -> Coord
goRight (x, y) = (x+1, y)
goLeft (x, y) = (x-1, y)
goUp (x, y) = (x, y+1)
goDown (x, y) = (x, y-1)

moveHead :: Coord -> Motion -> Coord
moveHead coord Up = goUp coord
moveHead coord Down = goDown coord
moveHead coord Right = goRight coord
moveHead coord Left = goLeft coord

moveTails :: Coord -> Coord -> Coord
moveTails h t_head = moveTail h t_head
    where moveTail (headX, headY) (tailX, tailY)
            | abs (headY - tailY) <= 1 && abs (headX - tailX) <= 1 = t_head
            | headY == tailY = (if tailX > headX then tailX - 1 else tailX + 1, tailY)
            | headX == tailX = (tailX, if tailY < headY then tailY + 1 else tailY - 1)
            | headX > tailX && headY > tailY = (tailX + 1, tailY + 1)
            | headX > tailX && headY < tailY = (tailX + 1, tailY - 1)
            | headX < tailX && headY > tailY = (tailX - 1, tailY + 1)
            | otherwise = (tailX - 1, tailY - 1)

move :: Grid -> Motion -> Grid
move (Grid h t) motion = 
    let
        new_head = moveHead h motion
    in Grid new_head (moveTails new_head (Prelude.head t) : t)

move10 :: Grid10 -> Motion -> Grid10
move10 (Grid10 prev knots) motion = 
    let
        l = folding [moveHead (Prelude.head knots) motion] (tail knots)
    in Grid10 (S.insert (Prelude.head l) prev) (reverse l)
  where
    folding :: [Coord] -> [Coord] -> [Coord]
    folding done [] = done
    folding done@(head : _) (next : rest) = folding (moveTails head next : done) rest

part :: MonadLogger m => Int -> [Motion] -> m Int
part p l =
    if p == 1
        then 
            let g = foldl move initGrid l
            in return $ (length . rmdups . tails) g
        else 
            let g = foldl move10 initGrid10 l
            in return $ (S.size . hs) g      

solvePart :: [Motion] -> Int -> IO ()
solvePart l n = runStdoutLoggingT (part n l) >>= (\x -> putStrLn $ "     part " <> show n <> ": " <> show x)

solve_day9 :: IO ()
solve_day9 = do
    divide dayNum
    s <- input
    let l = concatMap fromString $ lines s
    forM_ [1,2] $ solvePart l
