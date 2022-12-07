
module Day7 (solve_day7) where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M

inputPath :: String
inputPath = "./data/input_day7"

input :: IO String
input = readFile inputPath

initState :: MyState
initState = MyState [] M.empty

parse :: String -> Cmd
parse s
  | isPrefixOf "$ cd" s = Cd (drop 5 s)
  | isPrefixOf "$ ls" s = Ls
  | isPrefixOf "dir" s = Dir (drop 4 s)
  | otherwise = File size name
        where
            [x, y] = splitOn " " s
            size = read x :: Int
            name = y

data Cmd =
        Cd String
      | Ls
      | Dir String
      | File Int String
      deriving (Show)

data MyState = MyState {
      dir :: [String]
    , dirMap :: Map [String] Int
} deriving (Show)


addKey :: Map [String] Int -> [String] -> Int -> Map [String] Int
addKey prevMap key count =
  case M.lookup key prevMap of
    Nothing -> M.insert key count prevMap
    Just x -> M.insert key (x + count) prevMap

app :: MyState -> Cmd -> MyState
app prevState cmd =
  case cmd of
    Cd d -> if d == ".."
      then prevState { dir = tail (dir prevState)}
      else prevState { dir = d : dir prevState}
    Ls -> prevState
    Dir newDirName -> prevState
    File size _ ->
      let
          allDirs = dir prevState
          newDirMap = foldl (\mp d -> addKey mp d (fromIntegral size)) (dirMap prevState) (init $ tails allDirs)
      in
          prevState { dirMap = newDirMap}

solve :: Int -> String -> Int
solve part s =
    let
        cmds_str = lines s
        cmds = map parse cmds_str
        dirs = dirMap $ foldl app initState cmds
    in
        if part == 1
        then sum $ filter (<=100000) (M.elems dirs)
        else
            let
                allDirSizes = sort (M.elems dirs)
                usedSpace = last allDirSizes
                currentUnusedSpace = 70000000 - usedSpace
                mIn = find (\i -> currentUnusedSpace + i >= 30000000) allDirSizes
            in case mIn of
                 Just x -> x
                 Nothing -> 0

solve_day7 :: IO ()
solve_day7 = do
    putStr "     part1: "
    input >>= print . solve 1
    putStr "     part2: "
    input >>= print . solve 2
