{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day6 (solve_day6) where

import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as M

inputPath :: String
inputPath = "./data/input_day6"

input :: IO String
input = readFile inputPath

incKey :: Map Char Word -> Char -> Map Char Word
incKey prevMap key = addKey prevMap key 1

decKey :: Map Char Word -> Char -> Map Char Word
decKey prevMap key = 
  case M.lookup key prevMap of
    Nothing -> prevMap
    Just 0 -> M.delete key prevMap
    Just 1 -> M.delete key prevMap
    Just x -> M.insert key (x - 1) prevMap

addKey :: Map Char Word -> Char -> Word -> Map Char Word
addKey prevMap key count = 
  case M.lookup key prevMap of
    Nothing -> M.insert key count prevMap
    Just x -> M.insert key (x + count) prevMap

solve :: Int -> String -> Int
solve numChars s =
    let (firstChars, rest) = splitAt (numChars-1) s
        seq = Seq.fromList firstChars
        occ = foldl incKey M.empty firstChars
    in f numChars (numChars, seq, occ) rest

f :: Int -> (Int, Seq.Seq Char, Map Char Word) -> [Char] -> Int
f numChars (count, seq, occ) (c : cs) = 
  let 
    (first Seq.:< rest) = Seq.viewl seq 
    occ' = incKey occ c  
  in if M.size occ' == numChars
      then count
      else f numChars (count + 1, rest Seq.|> c, decKey occ' first) cs
   
solve_day6 :: IO ()
solve_day6 = do
    putStr "     part1: "
    input >>= print . solve 4
    putStr "     part2: "
    input >>= print . solve 14
