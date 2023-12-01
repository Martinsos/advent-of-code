module Day01 where

import Control.Arrow (first)
import Data.Function ((&))
import Data.List (find, isPrefixOf)
import Paths_aoc2023 (getDataFileName)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  putStrLn $ "Part one: " <> show (sum $ findCalibrationValue numberSpellings <$> inputLines)
  putStrLn $ "Part two: " <> show (sum $ findCalibrationValue (numberSpellings <> wordSpellings) <$> inputLines)
  where
    numberSpellings = zip (show <$> [0 :: Int .. 9]) [0 ..]
    wordSpellings = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 ..]

type Line = String

type Spellings = [(String, Int)]

findCalibrationValue :: Spellings -> Line -> Int
findCalibrationValue spellings line = 10 * findFirstDigit spellings line + findLastDigit spellings line

findFirstDigit :: Spellings -> Line -> Int
findFirstDigit _ [] = error "No digit found -> this should never happen!"
findFirstDigit spellings s@(_ : cs) =
  find ((`isPrefixOf` s) . fst) spellings
    & maybe (findFirstDigit spellings cs) snd

findLastDigit :: Spellings -> Line -> Int
findLastDigit spellings = findFirstDigit (first reverse <$> spellings) . reverse
