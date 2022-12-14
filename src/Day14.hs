{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day14 (solve_day14) where

import Algorithm.Search (bfsM)
import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
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
dayNum = 14

input :: IO String
-- input = readFile $ "./data/input_day" <> show dayNum
input = readFile $ "./data/test"

type Pos = (Int, Int)
data Element = Air | Rock | Sand | Start

instance Show Element where
    show Air = "."
    show Rock = "#"
    show Sand = "O"
    show Start = "+"

data Path = InputPath [Pos] | FullPath [Pos] deriving (Show)

data Cave = C {
    caveMap :: Map Pos Element
} deriving (Show)


showLine :: Map Pos Element -> [Pos] -> String
showLine m l = (foldl f "" l) <> "\n"
            where
                f :: String -> Pos -> String
                f s p =
                    case Map.lookup p m of
                        Nothing -> ""
                        Just x -> let s' = reverse s
                                      s'' = (show x) <> s'
                                  in reverse s''

printCave :: Cave -> String
printCave (C m) =
    let ((a, b), (c,d)) = getCaveDimensions (C m)
        l = [(x, y) | x <- [a..c], y <- [d..b]]
        l' = groupBy (\(_,a) (_,b) -> a==b) $ sortOn snd l
    in concat $ map (showLine m) l'

initCave :: Cave
initCave = C Map.empty

unPair :: [((Int, Int),(Int, Int))] -> [(Int, Int)]
unPair [] = []
unPair (p:ps) = (fst p) : (snd p) : (unPair ps)

mkPairs :: [(Int, Int)] -> [(Int, Int)]
mkPairs [] = []
mkPairs (p1:[]) = [p1]
mkPairs (p1:p2:ps)
    | fst p1 == fst p2 = let
                            (a, b) = if snd p1 > snd p2 then (snd p2, snd p1) else (snd p1, snd p2)
                         in (zip (repeat $ fst p1) ([a..b])) <> (mkPairs (p2:ps))
    | otherwise = let
                    (a, b) = if fst p1 > fst p2 then (fst p2, fst p1) else (fst p1, fst p2)
                  in (zip ([a..b]) (repeat $ snd p2)) <> (mkPairs (p2:ps))

generateFullPath :: Path -> Path
generateFullPath (InputPath []) = FullPath []
generateFullPath (InputPath ps) =
    let
        pairs = mkPairs ps
        pairs' = rmdups pairs
    in FullPath pairs'

instance Parse Path where
    parser = listP
        where
            intP :: Parser Int
            listP :: Parser Path

            listP = InputPath <$> sepBy sep pairP
                where
                    pairP :: Parser (Int, Int)
                    pairP = ((,) <$> (intP <* charP ',') <*> intP)

                    sep = stringP " -> "

                    sepBy :: Parser String -> Parser (Int, Int) -> Parser [(Int, Int)]
                    sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

            intP = f <$> notNull (spanP isDigit)
                where f ds = read ds :: Int

toPath :: String -> Path
toPath = fromString

foldCave :: Cave -> (Int, Int) -> Cave
foldCave (C cave) p = C $ Map.insert p Rock cave

getCaveDimensions :: Cave -> (Pos, Pos)
getCaveDimensions (C m) =
    let
        l = Map.foldlWithKey (\ks k a -> k:ks) [] m
        minX = getMinX l
        maxX = getMaxX l
        maxY = getMaxY l
    in
        ((minX, maxY), (maxX, 0))
        where
            getMinX l = minimum $ map fst l
            getMaxX l = maximum $ map fst l
            getMaxY l = maximum $ map snd l

fillCave :: Cave -> (Pos, Pos) -> Cave
fillCave (C cave) ((a,b), (c,d)) =
    let
        allPos = [(x, y) | x <- [a..c], y <- [d..b]]
     in C $ Map.insert (500, 0) Start $ foldl fillCaveWith cave allPos
        where
            fillCaveWith :: Map Pos Element -> Pos -> Map Pos Element
            fillCaveWith m p =
                case Map.lookup p m of
                  Nothing -> Map.insert p Air m
                  Just x -> m

code1 :: MonadLogger m => String -> m Int
code1 s = do
    let l = map toPath $ lines s
    logDebugN (T.pack $ "\n Paths: " <> show l)
    let rez = map generateFullPath l
    let l' = concat $ map (\(FullPath r) -> r) rez
    let cave = foldl foldCave initCave l'
    logDebugN (T.pack $ "\n rez: " <> show cave)
    let dims = getCaveDimensions cave
    logDebugN (T.pack $ "\n dimensions: " <> show dims)
    let filledCave = fillCave cave dims
    logDebugN (T.pack $ "\n filledCave: \n" <> printCave filledCave)
    -- rez <- foldM foldLine 0 (zip [1,2..] l)
    return $ length l

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    -- runStdoutLoggingT (code2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day14 :: IO ()
solve_day14 = do
    divide dayNum
    s <- input
    run s
