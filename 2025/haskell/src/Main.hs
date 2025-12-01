module Main where

import Control.Monad (void)
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
    "01" : _ -> void day01
    "02" : _ -> void day02
    "03" : _ -> void day03
    "04" : _ -> void day04
    "05" : _ -> void day05
    "06" : _ -> void day06
    "07" : _ -> void day07
    "08" : _ -> void day08
    "09" : _ -> void day09
    "10" : _ -> void day10
    "11" : _ -> void day11
    "12" : _ -> void day12
    _ -> error "None or invalid day number provided."
