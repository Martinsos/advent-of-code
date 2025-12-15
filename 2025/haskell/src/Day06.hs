module Day06 where

import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List (intercalate)
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, shouldBe, specify)

day06 :: IO (Integer, Integer)
day06 = solve <$> (getDataFileName "day06-input.txt" >>= readFile)

solve :: String -> (Integer, Integer)
solve input = (calcMathWorksheet worksheet, calcMathWorksheet $ first rotateCCW <$> worksheet)
  where
    worksheet = parseMathWorksheet input

type MathWorksheet = [MathProblem]

type MathProblem = (DigitsBlock, Operator)

type DigitsBlock = [[Char]]

data Operator = Sum | Product deriving (Show, Eq)

calcMathWorksheet :: MathWorksheet -> Integer
calcMathWorksheet = sum . (calcMathProblem <$>)

calcMathProblem :: MathProblem -> Integer
calcMathProblem (digitsBlock, operator) = calc operator $ read <$> digitsBlock

calc :: Operator -> [Integer] -> Integer
calc Sum = sum
calc Product = product

parseMathWorksheet :: String -> MathWorksheet
parseMathWorksheet input = go emptyAccLines $ lines input
  where
    go :: [String] -> [String] -> MathWorksheet
    go accLines leftoverLines
      | all null leftoverLines =
          mathProblemFromAccLines : []
      | all (isSpace . head) leftoverLines =
          mathProblemFromAccLines : go emptyAccLines (tail <$> leftoverLines)
      | otherwise =
          go (zipWith (<>) accLines (pure . head <$> leftoverLines)) (tail <$> leftoverLines)
      where
        mathProblemFromAccLines = (init accLines, parseOperator $ last accLines)

    emptyAccLines = repeat []

    parseOperator :: String -> Operator
    parseOperator =
      ( \case
          "*" -> Product
          "+" -> Sum
          op -> error $ "unsupported operator " <> op
      )
        . filter (not . isSpace)

rotateCCW :: [[a]] -> [[a]]
rotateCCW = foldr (zipWith (:) . reverse) (repeat [])

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

  describe "parseMathWorksheet" $ do
    specify "example input" $
      parseMathWorksheet exampleInputText
        `shouldBe` [ (["123", " 45", "  6"], Product),
                     (["328", "64 ", "98 "], Sum),
                     ([" 51", "387", "215"], Product),
                     (["64 ", "23 ", "314"], Sum)
                   ]

  specify "rotateCCW" $ do
    let test (m1 :: [[Int]], m2) = rotateCCW m1 `shouldBe` m2
    test
      ( [ [1, 2, 3]
        ],
        [ [3],
          [2],
          [1]
        ]
      )
    test
      ( [ [1],
          [2],
          [3]
        ],
        [ [1, 2, 3]
        ]
      )
    test
      ( [ [1, 2, 3],
          [4, 5, 6]
        ],
        [ [3, 6],
          [2, 5],
          [1, 4]
        ]
      )

  describe "solve" $ do
    specify "example input" $
      solve exampleInputText `shouldBe` (4277556, 3263827)

  specify "day06" $ do
    day06 >>= (`shouldBe` (5524274308182, 8843673199391))
