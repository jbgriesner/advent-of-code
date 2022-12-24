module Day22 (solve_day22) where


import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Utils

import qualified Data.Vector as V

dayNum :: Int
dayNum = 22

-- |

code :: MonadLogger m => Int -> String -> m Int
code k s
    | k == 1 = do
        return $ 1
    | otherwise = do
        return $ 2

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day22 :: IO ()
solve_day22 = do
    divide dayNum
    s <- input dayNum
    -- s <- inputest
    run s
