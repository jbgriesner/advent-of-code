{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day16 (solve_day16) where

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
import Data.Char           (isAlpha)

dayNum :: Int
dayNum = 16

-- | Parse Input Tools
newtype Valve = Valve String deriving (Eq)

instance Show Valve where 
    show (Valve s) = s

data LineType = L {
    valve :: Valve,
    flow :: Int,
    tunnels :: [Valve]
} deriving (Show)

instance Parse LineType where
    parser = (L <$> valveP) >>> numberP >>> valvesP
        where
            sepBy :: Parser String -> Parser Valve -> Parser [Valve]
            sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []  

            valvesP :: Parser [Valve]
            valvesP = (stringP "; tunnel " <|> stringP "; tunnels ") *> ((stringP "lead " <|> stringP "leads ") *> ((stringP "to valve "<|> stringP "to valves ") *> valves))
                where 
                    valves = sepBy sep vP
                    sep = stringP ", "

            vP :: Parser Valve
            vP = Valve <$> nameP

            valveP :: Parser Valve
            valveP = Valve <$> (stringP "Valve " *> nameP <* stringP " has flow rate=")

            nameP :: Parser String
            nameP = spanP isAlpha

            numberP :: Parser Int
            numberP = read <$> notNull (spanP isDigit) 

parseInput :: String -> [LineType]
parseInput s = map Utils.fromString $ lines s



-- |

code :: MonadLogger m => Int -> String -> m Int
code k s 
    | k == 1 = do
            let l = parseInput s
            return 1
            
    | otherwise = do
        return 0

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    -- runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day16 :: IO ()
solve_day16 = do
    divide dayNum
    -- s <- input dayNum
    s <- inputest 
    run s
