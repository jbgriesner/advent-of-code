{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day13 (solve_day13) where

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
dayNum = 13

input :: IO String
input = readFile $ "./data/input_day" <> show dayNum
-- input = readFile $ "./data/test"

data Packet =
  IntPacket Int |
  ListPacket [Packet]
  deriving (Show, Eq)

instance Parse Packet where
    parser = (listP <|> intP)
        where
            listP, intP :: Parser Packet

            listP = ListPacket <$> (charP '[' *> elements <* charP ']')
                where
                    elements = sepBy sep (listP <|> intP)
                    sep = charP ','

            intP = f <$> notNull (spanP isDigit)
                where f ds = IntPacket $ read ds

            sepBy :: Parser Char -> Parser Packet -> Parser [Packet]
            sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

toPacketPair :: String -> (Packet, Packet)
toPacketPair s =
    let
        l = lines s
        l' = map fromString l
    in (head l', last l')

foldLine :: (MonadLogger m) => Int -> (Int, (Packet, Packet)) -> m Int
foldLine prev (index, (p1, p2)) = do
    let rightOrder = evalPackets p1 p2
    return $ if rightOrder == LT then prev + index else prev

evalPackets :: Packet -> Packet -> Ordering
evalPackets (IntPacket a) (IntPacket b) = compare a b
evalPackets (IntPacket a) b@(ListPacket _) = evalPackets (ListPacket [IntPacket a])  b
evalPackets a@(ListPacket _) (IntPacket b) = evalPackets a (ListPacket [IntPacket b])
evalPackets (ListPacket packets1) (ListPacket packets2) = case (packets1, packets2) of
    ([], []) -> EQ
    ([], _) -> LT
    (_, []) -> GT
    (a : rest1, b : rest2) ->
        let compareFirst = evalPackets a b
        in  if compareFirst == EQ
            then evalPackets (ListPacket rest1) (ListPacket rest2)
            else compareFirst

code2 :: (MonadLogger m) => String -> m Int
code2 inputs = do
    let l = map toPacketPair $ splitOn "\n\n" inputs
        divider1 = ListPacket [ListPacket [IntPacket 2]]
        divider2 = ListPacket [ListPacket [IntPacket 6]]
        newInputs = (divider1, divider2) : l
        sortedPackets = sortBy evalPackets $ concat (pairToList <$> newInputs)
        i1 = elemIndex divider1 sortedPackets
        i2 = elemIndex divider2 sortedPackets
    case (i1, i2) of
        (Just index1, Just index2) -> return $ (index1 + 1) * (index2 + 1)
        _ -> return (-1)
    where
        pairToList (a, b) = [a, b]

code1 :: MonadLogger m => String -> m Int
code1 s = do
    let l = map toPacketPair $ splitOn "\n\n" s
    --  logDebugN (T.pack $ "\n Packets: " <> show l)
    rez <- foldM foldLine 0 (zip [1,2..] l)
    return rez

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day13 :: IO ()
solve_day13 = do
    divide dayNum
    s <- input
    run s
