module Main where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "01" : _ -> day01
    "02" : _ -> day02
    "03" : _ -> day03
    "04" : _ -> day04
    "05" : _ -> day05
    "06" : _ -> day06
    "07" : _ -> day07
    "08" : _ -> day08
    "09" : _ -> day09
    "10" : _ -> day10
    "11" : _ -> day11
    "12" : _ -> day12
    _ -> error "None or invalid day number provided."
