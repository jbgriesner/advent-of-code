{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day15 (solve_day15) where

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
dayNum = 15

input :: IO String
input = readFile $ "./data/input_day" <> show dayNum
-- input = readFile $ "./data/test"


code :: MonadLogger m => Int -> String -> m Int
code k s = do
    return 1

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day15 :: IO ()
solve_day15 = do
    divide dayNum
    s <- input
    run s
