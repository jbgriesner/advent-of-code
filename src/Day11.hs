{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day11 (solve_day11) where

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
dayNum = 11

input :: IO String
input = readFile $ "./data/input_day" <> show dayNum
-- input = readFile $ "./data/test"

data MonkeyRule = MR {
    id :: Int,
    items :: [Int],
    operation :: Int -> Int,
    test :: Int,
    sendTrueTo :: Int,
    sendFalseTo :: Int
}

instance Show MonkeyRule where
    show (MR x _ _ a b c) =
        "\nMR: { id->" <> show x
        <> ", test: " <> show a
        <> ", trueTo:" <> show b
        <> ", falseTo:" <> show c
        <> " }\n"

data Exp =
       Val Int
     | Old
     | Add Exp Exp
     | Sub Exp Exp
     | Mul Exp Exp
     | Div Exp Exp
    deriving (Show, Eq)

type Env = Int

instance Parse Exp where
    parser = ((oldP <|> numP) >==> symbolP) $$ (oldP <|> numP)
        where
            numP, oldP :: Parser Exp

            symbolP :: Parser (Exp -> Exp -> Exp)
            symbolP =
                    (stringP " * " *> pure Mul)
                <|> (stringP " / " *> pure Div)
                <|> (stringP " + " *> pure Add)
                <|> (stringP " - " *> pure Sub)

            oldP = stringP "old" *> pure Old
            numP = f <$> notNull (spanP isDigit)
                where f ds = Val $ read ds

toMonkey :: String -> MonkeyRule
toMonkey s =
    let
        l = lines s
        exp = stringToExp $ splitOn "new = " (l!!2) !! 1
    in MR {
        id = read $ init (splitOn " " (l!!0) !! 1),
        items = map read $ splitOn ", " (splitOn ": " (l!!1) !! 1),
        operation = \x -> eval x exp,
        test = read (splitOn "divisible by " (l!!3) !! 1),
        sendTrueTo = read (splitOn "to monkey " (l!!4) !! 1),
        sendFalseTo = read (splitOn "to monkey " (l!!5) !! 1)
    } where
        stringToExp :: String -> Exp
        stringToExp s = fromString s

        eval :: Env -> Exp -> Int
        eval e (Val v) = v
        eval e Old = e
        eval e (Add a b) = (eval e a) + (eval e b)
        eval e (Sub a b) = (eval e a) - (eval e b)
        eval e (Mul a b) = (eval e a) * (eval e b)
        eval e (Div a b) = (eval e a) `div` (eval e b)

getItems :: Map Int [Int] -> MonkeyRule -> Map Int [Int]
getItems m mr =
    let i = items mr
        idx = id mr
    in Map.insert idx i m

proceedMonkey :: MonadLogger m => Int -> (Map Int [Int], Map Int Int) -> MonkeyRule -> m (Map Int [Int], Map Int Int)
proceedMonkey mmm itemsMap mr =
    let
        itemsMonkey = (fst itemsMap) ! (id mr)
        numberInspections = length itemsMonkey
        previousInspections = (snd itemsMap) ! (id mr)
        -- newWorryLevels = map (`div` 3) $ map (operation mr) itemsMonkey
        newWorryLevels = map (`mod` mmm) $ map (operation mr) itemsMonkey
        updatedMap = Map.insert (id mr) [] (fst itemsMap)
        divisible = test mr
        trueTo = sendTrueTo mr
        falseTo = sendFalseTo mr

        f :: (Map Int [Int], Map Int Int) -> Int -> (Map Int [Int], Map Int Int)
        f m x = if x `mod` divisible == 0
                then (Map.insert trueTo (x : ((fst m) ! trueTo)) (fst m), Map.insert (id mr) (numberInspections+previousInspections) (snd m))
                else (Map.insert falseTo (x : ((fst m) ! falseTo)) (fst m), Map.insert (id mr) (numberInspections+previousInspections) (snd m))
    in do
      --  logDebugN (T.pack $ "\n itemsMonkey --| " <> show itemsMonkey)
      --  logDebugN (T.pack $ "newWorryLevels --| " <> show newWorryLevels)
      --  logDebugN (T.pack $ "updatedMap --| " <> show updatedMap)
      --  logDebugN (T.pack $ "trueTo --| " <> show trueTo)
      --  logDebugN (T.pack $ "falseTo --| " <> show falseTo)
        return $ foldl f (updatedMap, snd itemsMap) newWorryLevels

app :: MonadLogger m => Int -> Int -> [MonkeyRule] -> (Map Int [Int], Map Int Int) -> m (Map Int [Int], Map Int Int)
app mmm k mrs itemsMap = iterateM' (k-1) app' itemsMap
    where
        app' :: MonadLogger m => (Map Int [Int], Map Int Int) -> m (Map Int [Int], Map Int Int)
        app' itemsMap = do
            -- logDebugN (T.pack ".")
            foldM (proceedMonkey mmm) itemsMap mrs

        iterateM' 0 f a = f a
        iterateM' n f a = f a >>= iterateM' (n-1) f

code1 :: MonadLogger m => String -> m Int
code1 s = do
    let monkeyRules = map toMonkey $ splitOn "\n\n" s
    let mmm = foldl (*) 1 $ map test monkeyRules
    let initMap = foldl getItems Map.empty monkeyRules
    -- rounds <- forM [1..2] (\k -> app mmm k monkeyRules (initMap, Map.map (const 0) initMap))
    rounds <- app mmm 10000 monkeyRules (initMap, Map.map (const 0) initMap)

    -- logDebugN (T.pack "\n")
    -- logDebugN (T.pack $ "mmm --| " <> show mmm)
    -- logDebugN (T.pack "\n")
    -- logDebugN (T.pack "\n")
    -- logDebugN (T.pack $ "monkeyRules --| " <> show monkeyRules)
    -- logDebugN (T.pack "\n")
    -- logDebugN (T.pack "\n")
    -- logDebugN (T.pack $ "initMap --| " <> show initMap)
    -- logDebugN (T.pack "\n")
    -- logDebugN (T.pack "\n")
    -- logDebugN (T.pack $ "rounds --| " <> show rounds)
    -- logDebugN (T.pack "\n")
    -- return $ get2Max (map snd $ Map.assocs $ snd $ last rounds)
    return $ get2Max (map snd $ Map.assocs $ snd rounds)

solve:: String -> IO ()
solve s = do
    runStdoutLoggingT (code1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)

solve_day11 :: IO ()
solve_day11 = do
    divide dayNum
    s <- input
    solve s
