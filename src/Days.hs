module Days (runDays) where

import Common.Day (Day)
import qualified Common.Day.IO
import Day1 (solve_day1)
import Day2 (solve_day2)
import Day3 (solve_day3)
import Day4 (solve_day4)
import Day5 (solve_day5)
import Day6 (solve_day6)
import Day7 (solve_day7)
import Day8 (solve_day8)
import Day9 (solve_day9)
import Day10 (solve_day10)
import Day11 (solve_day11)
import Day12 (solve_day12)
import Day13 (solve_day13)
import Day14 (solve_day14)
import Day15 (solve_day15)
import Day16 (solve_day16)

allDays :: [Day]
allDays =
  [ day01part1,
    day01part2,
    day02part1,
    day02part2,
    day03part1,
    day03part2,
    day04part1,
    day04part2,
    day05part1,
    day05part2,
    day06part1,
    day06part2,
    day07part1,
    day07part2,
    day08part1,
    day08part2,
    day09part1,
    day09part2,
    day10part1,
    day10part2
  ]

runDays :: [String] -> IO ()
runDays = Common.Day.IO.runDays allDays
