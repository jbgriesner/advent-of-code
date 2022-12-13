{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day12 (solve_day12) where

import Algorithm.Search (bfsM)
import Data.Char
import Data.List hiding (tails)
import Utils
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Control.Monad
import qualified Data.Text as T
import Prelude hiding (id, Right, Down, Left, Up)
import qualified Data.Set as S
import Control.Monad.State
import Data.Char (isDigit)
import Control.Applicative (Alternative, empty, (<|>))
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map (Map, (!))

dayNum :: Int
dayNum = 12

input :: IO String
input = readFile $ "./data/input_day" <> show dayNum
-- input = readFile $ "./data/test"

data Matrix = Matrix {
    start :: (Int, Int),
    end :: (Int, Int),
    rows :: [[Int]]
} deriving (Show, Eq, Ord)

getRow :: Int -> Matrix -> [Int]
getRow n (Matrix _ _ m) = m !! n

getCol :: Int -> Matrix -> [Int]
getCol n (Matrix s e m) =
    let
        m' = transpose m
    in getRow n (Matrix s e m')

getElem :: Int -> Int -> Matrix -> Int
getElem x y m = getRow y m !! x

f :: Int -> [Int] -> Int
f k y = case elemIndex k y of
            Just x -> x
            Nothing -> -1

findStart :: [[Int]] -> (Int, Int)
findStart l =
    let
        t = map (f 83) l
        u = zip t [0..]
     in head $ filter (\(r, _) -> r /= -1) u

findEnd :: [[Int]] -> (Int, Int)
findEnd l =
    let
        t = map (f 69) l
        u = zip t [0..]
     in head $ filter (\(r, _) -> r /= -1) u

getNeighbors :: Matrix -> (Int, Int) -> [(Int, Int)]
getNeighbors (Matrix _ _ g) (x, y) = catMaybes [maybeUp, maybeDown, maybeLeft, maybeRight]
  where
    (maxX, maxY) = ((length $ head g) - 1, (length g) - 1)
    maybeUp = if y > 0 then Just (x, y - 1) else Nothing
    maybeDown = if y < maxY then Just (x, y + 1) else Nothing
    maybeLeft = if x > 0 then Just (x - 1, y) else Nothing
    maybeRight = if x < maxX then Just (x + 1, y) else Nothing

validMoves :: (MonadLogger m) => Matrix -> (Int, Int) -> m [(Int, Int)]
validMoves grid current = do
  let neighbors = getNeighbors grid current
      currentHeight = getElem (fst current) (snd current) grid
      result = filter (neighborTest currentHeight) neighbors
  do
    -- logDebugN (T.pack $ "-- | validMoves | --")
    -- logDebugN (T.pack $ "      -- | current: " <> show current)
    -- logDebugN (T.pack $ "      -- | currentHeight: " <> show currentHeight)
    -- logDebugN (T.pack $ "      -- | validMoves: " <> show result)
    return result
  where
    neighborTest currentHeight newCoord =
      let newHeight = getElem (fst newCoord) (snd newCoord) grid
      in newHeight - currentHeight <= 1

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replace (n-1) newVal xs

postProcessGrid :: (MonadLogger m) => Matrix -> m Matrix
postProcessGrid g =
    let (sx, sy) = start g     
        (ex, ey) = end g

        row1 = getRow sy g
        newRow1 = replace sx (ord 'a') row1
        
        row2 = getRow ey g
        newRow2 = if ey == sy
            then replace ex (ord 'z') newRow1
            else replace ex (ord 'z') row2

        newRows = if ey == sy 
            then replace sy newRow2 $ rows g
            else replace sy newRow1 $ rows g
        newRows2 = if ey == sy
            then newRows
            else replace ey newRow2 newRows
    in do       
        -- logDebugN (T.pack $ "-- | postProcessGrid | --")
        -- logDebugN (T.pack $ "      -- | row2: " <> show row2)
        -- logDebugN (T.pack $ "      -- | newRow2: " <> show newRow2)
        -- logDebugN (T.pack $ "      -- | start: " <> show (sx, sy))
        -- logDebugN (T.pack $ "      -- | end: " <> show (ex, ey))
        -- logDebugN (T.pack $ "      -- | grid: " <> show (take 3 $ reverse $ take 22 newRows2))
        return $ Matrix (sx, sy) (ex, ey) newRows2

code1 :: MonadLogger m => String -> m Int
code1 s = do
    let l = lines s
    let m = map (map ord) l
    let start = findStart m
    let end = findEnd m
    let mat = Matrix start end m
    newGrid <- postProcessGrid mat

    result <- bfsM (validMoves newGrid) (\c -> return (c == end)) start
  
    -- logDebugN (T.pack $ "\n" <> show m)
    -- logDebugN (T.pack $ "\n   start: " <> show start)
    -- logDebugN (T.pack $ "\n   end: " <> show end)
    -- logDebugN (T.pack $ "\n   newGrid: " <> show newGrid)
    -- logDebugN (T.pack $ "\n   result: " <> show result)
    return $ length $ fromJust $ result

remove97 :: (Int, [(Int, Int)]) -> (Int, [(Int, Int)])
remove97 (x, l) = (x, filter (\(_, x) -> x == 97) l)

addY :: (Int, [(Int, Int)]) -> [(Int, Int)]
addY (y, l) = map (\(x, _) -> (x, y)) l

findAllStarts :: [[Int]] -> [(Int, Int)]
findAllStarts l =
    let
        tt = map (zip [0..]) l
        ttt = zip [0..] tt
        tttt = map remove97 ttt
     in concatMap addY tttt

applyBfsWithThisStart :: MonadLogger m => Matrix -> (Int, Int) -> m Int
applyBfsWithThisStart grid start' = 
    let 
        m' = grid { start = start'}
    in do
        logDebugN (T.pack $ "-- | applyBfsWithThisStart | --")
        logDebugN (T.pack $ "      -- | trying this start: " <> show start')
        result <- bfsM (validMoves m') (\c -> return (c == (end grid))) start'
        let rez = case result of            
                    Just x -> length x
                    Nothing -> 11111111111
        logDebugN (T.pack $ "      -- | result length is: " <> show rez)
        return rez

code2 :: MonadLogger m => String -> m Int
code2 s = do
    let l = lines s
    let m = map (map ord) l
    let start = findStart m
    let end = findEnd m
    let mat = Matrix start end m
    newGrid <- postProcessGrid mat
    let allStarts = findAllStarts m
    -- result <- bfsM (validMoves newGrid) (\c -> return (c == end)) start
    allResults <- forM allStarts (\x -> applyBfsWithThisStart newGrid x)
    -- logDebugN (T.pack $ "\n" <> show m)
    logDebugN (T.pack $ "\n   allResults: " <> show allResults)
    -- logDebugN (T.pack $ "\n   end: " <> show end)
    -- logDebugN (T.pack $ "\n   newGrid: " <> show newGrid)
    -- logDebugN (T.pack $ "\n   result: " <> show result)
    return $ minimum allResults
         
run :: String -> IO ()
run s = do
    runStdoutLoggingT (code1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day12 :: IO ()
solve_day12 = do
    divide dayNum
    s <- input
    run s
