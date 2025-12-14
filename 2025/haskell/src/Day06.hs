module Day06 where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Void
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, shouldBe, specify)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

day06 :: IO (Integer, Integer)
day06 = do
  input <- getDataFileName "day06-input.txt" >>= readFile
  return (solvePart1 input, solvePart2 input)

solvePart1 :: String -> Integer
solvePart1 input = uncurry sumOfAllProblems numbersAndOperators
  where
    numbersAndOperators = parseInputPart1 input

    sumOfAllProblems :: [[Integer]] -> [Operator] -> Integer
    sumOfAllProblems numbersRows operatorsRow
      | all null numbersRows && null operatorsRow = 0
      | otherwise =
          calc (head operatorsRow) (head <$> numbersRows)
            + sumOfAllProblems (tail <$> numbersRows) (tail operatorsRow)

type Parser = P.Parsec Void String

parseInputPart1 :: String -> ([[Integer]], [Operator])
parseInputPart1 = either (error . P.errorBundlePretty) id . P.parse inputP ""
  where
    inputP = do
      numbersRows <- P.some $ P.try (numbersRowP <* P.eol)
      operatorsRow <- operatorsRowP
      return (numbersRows, operatorsRow)

    numbersRowP :: Parser [Integer]
    numbersRowP = P.hspace *> P.some (L.decimal <* P.hspace)

    operatorsRowP :: Parser [Operator]
    operatorsRowP = P.hspace *> P.some (operatorP <* P.hspace)

    operatorP :: Parser Operator
    operatorP = (Product <$ P.char '*') P.<|> (Sum <$ P.char '+')

solvePart2 :: String -> Integer
solvePart2 input = uncurry (sumOfAllProblems []) numberLinesAndOperators
  where
    numberLinesAndOperators = parseInputPart2 input

    sumOfAllProblems :: [Integer] -> [[Char]] -> [Operator] -> Integer
    sumOfAllProblems accNums numberLines operators
      | all null numberLines = calc (head operators) accNums
      | all (isSpace . head) numberLines =
          calc (head operators) accNums + sumOfAllProblems [] (tail <$> numberLines) (tail operators)
      | otherwise =
          sumOfAllProblems (read (head <$> numberLines) : accNums) (tail <$> numberLines) operators

parseInputPart2 :: String -> ([[Char]], [Operator])
parseInputPart2 input = (init inputLines, parseOperators $ last inputLines)
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

  describe "parseInputPart1" $ do
    specify "example input" $
      parseInputPart1 exampleInputText
        `shouldBe` ([[123, 328, 51, 64], [45, 64, 387, 23], [6, 98, 215, 314]], [Product, Sum, Product, Sum])

  describe "parseInputPart2" $ do
    specify "example input" $
      parseInputPart2 exampleInputText
        `shouldBe` (["123 328  51 64 ", " 45 64  387 23 ", "  6 98  215 314"], [Product, Sum, Product, Sum])

  describe "solvePart1" $ do
    specify "example input" $
      solvePart1 exampleInputText `shouldBe` 4277556

  describe "solvePart2" $ do
    specify "example input" $
      solvePart2 exampleInputText `shouldBe` 3263827

  specify "day06" $ do
    day06 >>= (`shouldBe` (5524274308182, 8843673199391))
