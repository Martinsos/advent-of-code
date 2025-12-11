module Day06 where

import Data.List (intercalate)
import Data.Void
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, shouldBe, specify)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

day06 :: IO (Integer, Integer)
day06 = solve <$> (getDataFileName "day06-input.txt" >>= readFile)

solve :: String -> (Integer, Integer)
solve input = (uncurry sumOfAllProblems $ numbersAndOperators, undefined)
  where
    numbersAndOperators = parseInput input

    sumOfAllProblems :: [[Integer]] -> [Operator] -> Integer
    sumOfAllProblems numbersRows operatorsRow
      | all null numbersRows && null operatorsRow = 0
      | otherwise =
          calc (head operatorsRow) (head <$> numbersRows)
            + sumOfAllProblems (tail <$> numbersRows) (tail operatorsRow)

type Parser = P.Parsec Void String

parseInput :: String -> ([[Integer]], [Operator])
parseInput = either (error . P.errorBundlePretty) id . P.parse inputP ""
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
        `shouldBe` ([[123, 328, 51, 64], [45, 64, 387, 23], [6, 98, 215, 314]], [Product, Sum, Product, Sum])

  describe "solve" $ do
    specify "example input" $
      fst (solve exampleInputText) `shouldBe` 4277556

  specify "day06" $ do
    day06 >>= (`shouldBe` 5524274308182) . fst
