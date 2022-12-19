module Day19 (solve_day19) where

import Data.List
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Utils (input, inputest, Parse (..), Parser (..), charP)
import Utils

dayNum :: Int
dayNum = 19

code :: MonadLogger m => Int -> String -> m Int
code k s
    | k == 1 = do
        return 1
    | otherwise = do
        return 2

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day19 :: IO ()
solve_day19 = do
    divide dayNum
    -- s <- input dayNum
    s <- inputest
    run s
