module Main (main) where

import Days

divider :: Int -> IO ()
divider s = putStrLn $ "--- result of day " ++ show s ++ " is:"

main :: IO ()
main = do
    divider 1
    solve_day1
    divider 2
    solve_day2
    divider 3
    solve_day3
    divider 4
    solve_day4
    divider 5
    solve_day5
