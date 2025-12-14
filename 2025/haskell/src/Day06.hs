module Day06 where

import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List (intercalate)
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, shouldBe, specify)

day06 :: IO (Integer, Integer)
day06 = do
  input <- getDataFileName "day06-input.txt" >>= readFile
  return (solvePart1 input, solvePart2 input)

solvePart1 :: String -> Integer
solvePart1 input = uncurry sumOfAllProblems numbersAndOperators
  where
    numbersAndOperators = first (((read <$>) . words) <$>) $ parseInput input

    sumOfAllProblems :: [[Integer]] -> [Operator] -> Integer
    sumOfAllProblems numbersRows operatorsRow
      | all null numbersRows && null operatorsRow = 0
      | otherwise =
          calc (head operatorsRow) (head <$> numbersRows)
            + sumOfAllProblems (tail <$> numbersRows) (tail operatorsRow)

solvePart2 :: String -> Integer
solvePart2 input = uncurry (sumOfAllProblems []) numberLinesAndOperators
  where
    numberLinesAndOperators = parseInput input

    sumOfAllProblems :: [Integer] -> [[Char]] -> [Operator] -> Integer
    sumOfAllProblems accNums numberLines operators
      | all null numberLines = calc (head operators) accNums
      | all (isSpace . head) numberLines =
          calc (head operators) accNums + sumOfAllProblems [] (tail <$> numberLines) (tail operators)
      | otherwise =
          sumOfAllProblems (read (head <$> numberLines) : accNums) (tail <$> numberLines) operators

parseInput :: String -> ([[Char]], [Operator])
parseInput input = (init inputLines, parseOperators $ last inputLines)
  where
    inputLines = lines input
    parseOperators = map parseOperator . filter (not . isSpace)
    parseOperator = \case
      '*' -> Product
      '+' -> Sum
      _unsupportedOperator -> error "unsupported operator"

data Operator = Sum | Product deriving (Show, Eq)

calc :: Operator -> [Integer] -> Integer
calc Sum = sum
calc Product = product

------------------------------------------------------------------------------------------

tests :: IO ()
tests = hspec $ do
  let exampleInputText =
        intercalate
          "\n"
          [ "123 328  51 64 ",
            " 45 64  387 23 ",
            "  6 98  215 314",
            "*   +   *   +  "
          ]

  describe "parseInput" $ do
    specify "example input" $
      parseInput exampleInputText
        `shouldBe` (["123 328  51 64 ", " 45 64  387 23 ", "  6 98  215 314"], [Product, Sum, Product, Sum])

  describe "solvePart1" $ do
    specify "example input" $
      solvePart1 exampleInputText `shouldBe` 4277556

  describe "solvePart2" $ do
    specify "example input" $
      solvePart2 exampleInputText `shouldBe` 3263827

  specify "day06" $ do
    day06 >>= (`shouldBe` (5524274308182, 8843673199391))
