{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
module Day10 (solve_day10) where

import Data.List hiding (tails)
import Utils
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Control.Monad
import qualified Data.Text as T
import Prelude hiding (Right, Down, Left, Up)
import Data.Char (isDigit)
import Control.Applicative ((<|>))
import Data.Maybe

dayNum :: Int
dayNum = 10

input :: IO String
input = readFile $ "./data/input_day" <> show dayNum
-- input = readFile $ "./data/test"

data Instructions = Noop | AddX Int deriving (Show)

instValue :: Parser Instructions
instValue = instNoop <|> instAddX
    where
        instAddX, instNoop, number, negativeNumber :: Parser Instructions
        instAddX = stringP "addx " *> (negativeNumber <|> number)

        instNoop = (\_ -> Noop) <$> stringP "noop"

        number = f <$> notNull (spanP isDigit)
            where f ds = AddX $ read ds

        negativeNumber = f <$> notNull (stringP "-" *> spanP isDigit)
            where f ds = AddX $ (-1) * read ds

data Register = Register {
    registerX :: Int,
    cycleNumber :: Int
} deriving (Show)

type History = [(Register, Instructions, String)]

initHistory :: History
initHistory = [(Register 1 1, Noop, "")]

cycles :: [Int]
cycles = take 500 $ 20 : f 20
    where f n = (n+40) : f (n+40)

code :: MonadLogger m => Int -> String -> m String
code n s
    | n == 1 = code1 s
    | otherwise = code2 s

p40::[a] -> [[a]]
p40 [] = []
p40 l = take 40 l : (p40 $ drop 40 l)

code2 :: MonadLogger m => String -> m String
code2 s = let
        instructions = map fst $ mapMaybe (runParser instValue) $ lines s
        l = foldl app initHistory instructions
        l'' = intercalate "\n" $ map (\x -> reverse $ reverse x <> "         ") $ p40 $ (\(_, _, z) -> reverse z) $ head l
    in do
        -- logDebugN (T.pack "\n")
        -- logDebugN (T.pack $ "instructions --| " <> show instructions)
        -- logDebugN (T.pack "\n")
        -- logDebugN (T.pack "\n")
        -- logDebugN (T.pack $ "history --| " <> show l)
        -- logDebugN (T.pack "\n")
        -- logDebugN (T.pack "\n")
        -- logDebugN (T.pack $ "output string --| " <> show ((\(x, y, z) -> z) $ head l) )
        -- logDebugN (T.pack "\n")
        -- logDebugN (T.pack "\n")
        -- logDebugN (T.pack $ "output reverse string --| " <> show ((\(x, y, z) -> reverse z) $ head l) )
        logDebugN (T.pack $ "\n l'':\n" <> l'')
        return $ show ((\(x, y, z) -> reverse z) $ head l)
        where
            app :: History -> Instructions -> History
            app l@((Register r c, _, stringOutput) : _) Noop =
                let
                    newChar = if ((c - 1) `mod` 40) `elem` [r - 1, r, r + 1] then '#' else '.'
                in (Register r (c+1), Noop, newChar : stringOutput) : l
            app l@((Register r c, _, stringOutput) : _) (AddX x) =
                let
                    newChar1 = if ((c - 1) `mod` 40) `elem` [r - 1, r, r + 1] then '#' else '.'
                    newChar2 = if (c `mod` 40) `elem` [r - 1, r, r + 1] then '#' else '.'
                in (Register (r+x) (c+2), AddX x, newChar2 : newChar1 : stringOutput) : (Register r (c+1), AddX x, newChar1 : stringOutput) : l

code1 :: MonadLogger m => String -> m String
code1 s =
    let
        instructions = map fst $ mapMaybe (runParser instValue) $ lines s
        l = foldl app initHistory instructions
        l' = filter (\(Register _ c, _, _) -> c `elem` cycles ) l
    in return $ show $ sum $ map (\(Register r c, _, _) -> r*c) l'
        where
            app :: History -> Instructions -> History
            app l@((Register r c, _, _) : _) Noop = (Register r (c+1), Noop, "") : l
            app l@((Register r c, _, _) : _) (AddX x) = (Register (r+x) (c+2), AddX x, "") : (Register r (c+1), AddX x, "") : l

solvePart :: String -> Int -> IO ()
solvePart s n = runStdoutLoggingT (code n s) >>= (\x -> putStrLn $ "     part " <> show n <> ": \n\n" <> show x)

solve_day10 :: IO ()
solve_day10 = do
    divide dayNum
    s <- input
    forM_ [1,2] $ solvePart s
