{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day14 (solve_day14) where

import Control.Concurrent
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
input = readFile $ "./data/input_day" <> show dayNum
-- input = readFile $ "./data/test"

type Pos = (Int, Int)
data Element = Air | Rock | Sand | Start deriving (Enum, Eq)

instance Show Element where
    show Air = "."
    show Rock = "#"
    show Sand = "O"
    show Start = "+"

data Path = InputPath [Pos] | FullPath [Pos] deriving (Show)

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

data Cave = C {
    caveMap :: Map Pos Element
} deriving (Show, Eq)

initCave :: Cave
initCave = C Map.empty

printCave :: Cave -> String
printCave (C m) =
    let ((a, b), (c,d)) = getCaveDimensions (C m)
        l = [(x, y) | x <- [a..c], y <- [d..b]]
        l' = groupBy (\(_,a) (_,b) -> a==b) $ sortOn snd l
    in concat $ map (showLine m) l'
        where
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

generateFullPath :: Path -> Path
generateFullPath (InputPath ps) = FullPath $ rmdups $ mkPairs ps
            where
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

foldCave :: Cave -> (Int, Int) -> Cave
foldCave (C cave) p = C $ Map.insert p Rock cave

getCaveInfiniteDimensions :: Cave -> (Pos, Pos)
getCaveInfiniteDimensions (C m) =
    let
        l = Map.foldlWithKey (\ks k a -> k:ks) [] m
        minX = (getMinX l) - maxY
        maxX = (getMaxX l) + maxY
        maxY = getMaxY l
    in
        ((minX, maxY), (maxX, 0))
        where
            getMinX l = minimum $ map fst l
            getMaxX l = maximum $ map fst l
            getMaxY l = 2+(maximum $ map snd l)

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

addSandUnit :: Cave -> Maybe Cave
addSandUnit (C m) = case nextValidPos m (500,0) of
                      Just x -> Just . C $ Map.insert x Sand m
                      Nothing -> Nothing
            where
                nextValidPos :: Map Pos Element -> Pos -> Maybe Pos
                nextValidPos m (x,y) =
                    do
                        r0 <- Map.lookup (500, 0) m
                        r <- Map.lookup (x,y+1) m
                        rg <- Map.lookup (x-1,y+1) m
                        rd <- Map.lookup (x+1,y+1) m
                        if r0 == Sand
                        then Nothing
                        else case r `elem` [Rock, Sand] of
                                    False -> nextValidPos m (x, y+1)
                                    True -> case rg `elem` [Rock, Sand] of
                                                    False -> nextValidPos m (x-1, y)
                                                    True -> case rd `elem` [Rock, Sand] of
                                                                        True -> Just (x,y)
                                                                        False -> nextValidPos m (x+1, y)

getIndex :: MonadLogger m => Int -> Cave -> m Int
getIndex k c = do
                -- logDebugN (T.pack $ "\n -- | Iteration: " <> show k)
                -- logDebugN (T.pack $ "\n      cave is: \n" <> printCave c)
                case addSandUnit c of
                    Nothing -> return k
                    Just c' -> getIndex (k+1) c'

addRocksBottom :: Cave -> (Pos, Pos) -> Cave
addRocksBottom (C cave) ((a,b), (c,d)) = C $ foldl f cave [a..c]
        where
            f :: Map Pos Element -> Int -> Map Pos Element
            f m p = Map.insert (p,b) Rock m

code :: MonadLogger m => Int -> String -> m Int
code k s = do
    let l = map toPath $ lines s
   --  logDebugN (T.pack $ "\n Paths: " <> show l)
    let rez = map generateFullPath l
    let l' = concat $ map (\(FullPath r) -> r) rez
    let cave = foldl foldCave initCave l'
   --  logDebugN (T.pack $ "\n cave: " <> show cave)
    let dims = if k == 1 then getCaveDimensions cave else getCaveInfiniteDimensions cave
   --  logDebugN (T.pack $ "\n dimensions: " <> show dims)
    let filledCave = if k == 1 then fillCave cave dims else addRocksBottom (fillCave cave dims) dims
   --  logDebugN (T.pack $ "\n filledCave: \n" <> printCave filledCave)
    o <- getIndex 0 filledCave
    -- rez <- foldM foldLine 0 (zip [1,2..] l)
    return o

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day14 :: IO ()
solve_day14 = do
    divide dayNum
    s <- input
    run s
